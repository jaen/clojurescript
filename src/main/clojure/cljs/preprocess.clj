(ns cljs.preprocess
  (:require [cljs.util :as util]
    ; [cljs.js-deps :as deps]
            [cljs.closure :as closure]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.env :as env]
            [cljs.js-deps :as deps])
  (:import [java.io File BufferedInputStream StringWriter]
           [java.net URL]
           [java.util.logging Level]
           [java.util List Random]
           [java.util.concurrent TimeUnit]
           [com.google.javascript.jscomp CompilerOptions CompilationLevel
                                         JavascriptModuleLoaderHelpers IJavascriptModuleLoader
                                         AbstractCompiler TransformAMDToCJSModule
                                         CompilerOptions$LanguageMode SourceMap$Format
                                         SourceMap$DetailLevel ClosureCodingConvention SourceFile
                                         Result JSError CheckLevel DiagnosticGroups
                                         CommandLineRunner AnonymousFunctionNamingPolicy
                                         JSModule JSModuleGraph SourceMap ProcessCommonJSModules
                                         JavascriptModuleLoaderHelpers ES6ModuleLoader AbstractCompiler
                                         TransformAMDToCJSModule ProcessEs6Modules CompilerInput]
           [com.google.javascript.rhino Node]
           [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]
           [java.nio.file Path Paths Files StandardWatchEventKinds WatchKey
                          WatchEvent FileVisitor FileVisitResult]
           [com.sun.nio.file SensitivityWatchEventModifier]
           [com.google.common.base Throwables]
           (cljs.js_deps IJavaScript)))

(declare is-new-es6-loader?)
(declare default-module-root)
;(declare PreprocessableJavaScriptFile)

(util/compile-if
  (.getConstructor ES6ModuleLoader
                   (into-array java.lang.Class
                               [java.util.List java.lang.Iterable]))
  (do (def is-new-es6-loader? true)
      (def default-module-root JavascriptModuleLoaderHelpers/DEFAULT_FILENAME_PREFIX))
  (def is-new-es6-loader? false))

(declare  is-old-es6-loader?)

(util/compile-if
  (.getConstructor ES6ModuleLoader
                   (into-array java.lang.Class
                               [AbstractCompiler java.lang.String]))
  (def is-old-es6-loader? true)
  (def is-old-es6-loader? false))

(declare can-convert-commonjs?)

(util/compile-if
  (and (.getConstructor ProcessCommonJSModules
                        (into-array java.lang.Class
                                    [com.google.javascript.jscomp.Compiler IJavascriptModuleLoader]))
       (or is-new-es6-loader? is-old-es6-loader?))
  (def can-convert-commonjs? true)
  (def can-convert-commonjs? false))

(declare can-convert-amd?)

(util/compile-if
  (and can-convert-commonjs?
       (.getConstructor TransformAMDToCJSModule
                        (into-array java.lang.Class
                                    [AbstractCompiler])))
  (def can-convert-amd? true)
  (def can-convert-amd? false))

(declare can-convert-es6?)

(util/compile-if
  (and (.getConstructor ProcessEs6Modules
                        (into-array java.lang.Class
                                    [com.google.javascript.jscomp.Compiler IJavascriptModuleLoader Boolean/TYPE]))
       (or is-new-es6-loader? is-old-es6-loader?))
  (def can-convert-es6? true)
  (def can-convert-es6? false))

;; ===

(defn get-js-module-root [js-file]
  (let [path (.getParent (io/file js-file))]
    (cond->> path
             (.startsWith path File/separator) (str ".")
             (not (.startsWith path (str "." File/separator))) (str "." File/separator)
             (not (.endsWith path File/separator)) (#(str % File/separator)))))


(defmulti js-source-file (fn [_ source] (class source)))

(defmethod js-source-file String [^String name ^String source]
  (SourceFile/fromCode name source))

(defmethod js-source-file File [_ ^File source]
  (SourceFile/fromFile source))

(defmethod js-source-file BufferedInputStream [^String name ^BufferedInputStream source]
  (SourceFile/fromInputStream name source))

(declare make-es6-loader)

(util/compile-if is-new-es6-loader?
  (defn make-es6-loader [source-files]
    (let [^List module-roots (list default-module-root)
          ^List compiler-inputs (map #(CompilerInput. %) source-files)]
      (ES6ModuleLoader. module-roots compiler-inputs)))
  (defn make-es6-loader [closure-compiler file]
    (let [module-root (get-js-module-root file)]
      (ES6ModuleLoader. closure-compiler module-root))))

(defn join-path [path-elements]
  (string/join File/separator path-elements))

(declare preprocess-source)

(defrecord PreprocessableJavaScriptFile [file url lib-spec opts provides requires source]
  deps/IJavaScript
  (-foreign? [this] false)
  (-closure-lib? [this] true)
  (-modular-lib? [this] true)
  (-url [this] url)
  (-provides [this] provides #_(:provides (deps/parse-js-ns (string/split-lines (deps/-source this)))))
  (-requires [this] requires #_(:requires (deps/parse-js-ns (string/split-lines (deps/-source this)))))
  (-source [this]
    source
    #_(if true ; (not-empty opts)
      (preprocess-source url lib-spec opts)
      (with-open [reader (io/reader url)]
        (slurp reader)))))

(defn relativise [root url]
  (let [file-url-as-uri (.toURI url)
        root-as-uri (.toURI root)]
    (.toString (.relativize root-as-uri file-url-as-uri))))

(defn wrap-with-spec [{:keys [root files main name module-type] :as lib-spec} opts url]
  (let [; opts (or (get-in (or (when-let [c env/*compiler*] @c) {}) [:options]) {})
        run-dir (io/file (System/getProperty "user.dir")) ; (io/file (util/output-directory (get-in @env/*compiler* [:options])))
        file-relative-path (relativise run-dir url)
        source (preprocess-source url lib-spec opts)
        parsed-ns (deps/parse-js-ns (string/split-lines source))
        pjs (->PreprocessableJavaScriptFile file-relative-path url lib-spec opts (:provides parsed-ns) (:requires parsed-ns) source)]
    ; (deps/-source pjs)
    pjs
    #_{:url url
       :file file-relative-path
       :module-type module-type}))

(defn get-all-lib-files [{:keys [root files main name module-type] :as lib} opts]
  (doall (mapcat (fn [path]
            (let [files (deps/find-resources (join-path [root path]) #(.endsWith % ".js"))]
              (map (partial wrap-with-spec lib opts)
                 files)))
          files)))

(defn get-all-lib-files-raw [{:keys [root files main name module-type] :as lib} opts]
  (doall (mapcat (fn [path]
            (let [files (deps/find-resources (join-path [root path]) #(.endsWith % ".js"))]
              (map (fn [url]
                     (let [working-dir (io/file #_(:root lib-spec) (System/getProperty "user.dir"))
                           file-relative-path (relativise working-dir url)]
                       {:file file-relative-path}))
                 files)))
          files)))


(defprotocol ILookupMap
  (lookup [this what])
  (inverted-lookup [this what]))

(defn parent-dir [path]
  (let [idx (.lastIndexOf path "/")]
    (if (> idx 0)
      (subs path 0 idx)
      path)))

(defn make-cjs-loader [lib-spec source-files all-lib-specs]
  (let [module-roots (apply list (into #{} (filter some? (map (comp parent-dir :root) all-lib-specs))))
        ^List compiler-inputs (map #(CompilerInput. ^SourceFile %) source-files)
        es6-loader (ES6ModuleLoader. module-roots compiler-inputs)
        lookup-map {}
        inverted-lookup-map {}]
    (reify
      IJavascriptModuleLoader
      (locateCommonJsModule [_ require-name context]
        (.locateCommonJsModule es6-loader require-name context))

      (locateEs6Module [_ module-name context]
        (.locateEs6Module es6-loader module-name context))

      (normalizeInputAddress [_ input]
        (.normalizeInputAddress es6-loader input))

      (toModuleName [_ filename]
        (.toModuleName es6-loader filename))

      (toModuleIdentifier [_ filename]
        (.toModuleName es6-loader filename))

      ILookupMap
      (lookup [_ what]
        (get lookup-map what))

      (inverted-lookup [_ what]
        (get inverted-lookup-map what)))))

(defn ^Node get-root-node [ijs closure-compiler]
  (let [^CompilerInput input (->> (deps/-source ijs)
                                  (js-source-file (:file ijs))
                                  (CompilerInput.))]
    (.getAstRoot input closure-compiler)))

(defn set-options [& args]
  (apply (resolve 'cljs.closure/set-options) args))

(defn make-convert-js-module-options [opts]
  (-> opts
      (select-keys
        [:closure-warnings :closure-extra-annotations :pretty-print
         :language-in :language-out])
      (set-options (CompilerOptions.))))

(defn get-source-files [lib-spec opts]
  (let [libs (filter map? (:libs opts))]
    (map #(js-source-file (:file %) (deps/-source %))
         (mapcat get-all-lib-files libs))))

(defn get-source-files-raw [lib-spec opts]
  (let [libs (filter map? (:libs opts))]
    (map #(js-source-file (:file %) (deps/-source %))
         (mapcat get-all-lib-files-raw libs opts))))

(defn make-closure-compiler [& args]
  (apply (resolve 'cljs.closure/make-closure-compiler) args))

(defn preprocess-source [url lib-spec opts]
  (let [source (with-open [reader (io/reader url)]
                 (slurp reader))
        working-dir (io/file #_(:root lib-spec) (System/getProperty "user.dir")) ; (io/file (util/output-directory (get-in @env/*compiler* [:options])))
        file-relative-path (relativise working-dir url)
        module-type (:module-type lib-spec)
        externs '()
        source-files (get-source-files-raw lib-spec opts)
        options (make-convert-js-module-options opts)
        closure-compiler (doto (make-closure-compiler)
                           (.init externs source-files options))
        libs (or (:libs opts) [])
        cjs-loader (if is-new-es6-loader?
                     (make-cjs-loader lib-spec source-files libs))
        cjs (ProcessCommonJSModules. closure-compiler cjs-loader)
        ^Node root (let [^CompilerInput input (->> (js-source-file file-relative-path source)
                                                   (CompilerInput.))]
                       (.getAstRoot input closure-compiler))
        ]
    (.process cjs nil root)
    (closure/report-failure (.getResult closure-compiler))
    (.toSource closure-compiler root)))

(defn preprocess [{:keys [root] :as lib-spec} opts]
  (let [;opts (:options env/*compiler*)
        libs (:libs opts)
        file-filter #(.endsWith % ".js")
        files (get-all-lib-files lib-spec opts)]
    (let [file (first files)]
      (deps/-source file))
    files))