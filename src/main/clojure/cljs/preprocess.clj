(ns cljs.preprocess
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.util :as util]
            [cljs.closure.options :as closure-options]
            [cljs.js-deps.resources-util :as find-res]
            [cljs.js-deps.ns-util :as ns-util]
            [cljs.js-deps.protocols :as deps-proto])
  (:import [java.io File]
           [java.util List]
           [java.util.logging Level]
           [java.io File BufferedInputStream]
           [com.google.javascript.rhino Node]
           [com.google.javascript.jscomp CompilerOptions JavascriptModuleLoaderHelpers IJavascriptModuleLoader
                                         AbstractCompiler TransformAMDToCJSModule SourceFile ProcessCommonJSModules
                                         JavascriptModuleLoaderHelpers ES6ModuleLoader AbstractCompiler
                                         TransformAMDToCJSModule ProcessEs6Modules CompilerInput]
           (java.net URI)))

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

(defrecord PreprocessedJavaScriptFile [file url lib-spec opts provides requires source]
  deps-proto/IJavaScript
  (-foreign? [this] false)
  (-closure-lib? [this] true)
  ; (-modular-lib? [this] true)
  (-url [this] url)
  (-provides [this] provides)
  (-requires [this] requires)
  (-source [this] source))

(declare make-es6-loader)

(util/compile-if is-new-es6-loader?
                 (defn make-es6-loader [source-files]
                   (let [^List module-roots (list default-module-root)
                         ^List compiler-inputs (map #(CompilerInput. %) source-files)]
                     (ES6ModuleLoader. module-roots compiler-inputs)))
                 (defn make-es6-loader [closure-compiler file]
                   (let [module-root (get-js-module-root file)]
                     (ES6ModuleLoader. closure-compiler module-root))))

(defn wrap-with-spec [{:as lib-spec} opts url]
  (let [working-dir (io/file (System/getProperty "user.dir"))
        source (with-open [reader (io/reader url)]
                 (slurp reader))
        lib-file-spec {:url url
                       :file (util/relativise working-dir url)
                       :closure-lib true
                       :modular-lib true
                       :lib-spec lib-spec
                       :opts opts
                       :source source}]
    lib-file-spec))

(defn get-all-lib-files [{:keys [root files] :as lib} opts]
  (let [resource-filter #(.endsWith % ".js")]
    (doall (mapcat (fn [path]
                     ;(println "LOOKING FOR RESOURCES FOR " root " " path)
                     (let [files (find-res/find-resources (util/join-path [root path]) resource-filter)]
                       (map (partial wrap-with-spec lib opts)
                            files)))
                   files))))

;(def get-all-lib-files (memoize get-all-lib-files'))

(defn parent-dir [path]
  (let [idx (.lastIndexOf path "/")]
    (if (> idx 0)
      (subs path 0 idx)
      path)))

(defn relative-path? [path]
  (if [path (.toString path)]
    (or (.startsWith path "../")
        (.startsWith path "./"))))

(defn make-lookup-map [lib-file-spec]
  (let [opts (:opts lib-file-spec)
        libs (or (:libs opts) [])]
    ))

(defn make-cjs-loader [lib-file-spec source-files]
  (let [opts (:opts lib-file-spec)
        libs (or (:libs opts) [])
        lib-root-uri (.toURI (io/file (get-in lib-file-spec [:lib-spec :root])))
        module-roots (apply list (into #{} (filter some? (map (comp parent-dir :root) libs))))
        ^List compiler-inputs (map #(CompilerInput. ^SourceFile %) source-files)
        es6-loader (ES6ModuleLoader. module-roots compiler-inputs)
        lookup-map (make-lookup-map lib-file-spec)
        inverted-lookup-map {}]
    (reify
      IJavascriptModuleLoader
      (locateCommonJsModule [_ require-name context]
        (let [lookup-value (if (relative-path? require-name)
                             (let [context-name (.getName context)
                                   context-uri (.toURI (io/file (System/getProperty "user.dir") context-name))
                                   resolved (.resolve context-uri require-name)]
                               ;(println "REQUIRE-NAME=" require-name " CONTEXT-URI=" context-uri " RESOLVED=" resolved)
                               resolved)
                             require-name)]
          )
        (.locateCommonJsModule es6-loader require-name context))

      (locateEs6Module [_ module-name context]
        (.locateEs6Module es6-loader module-name context))

      (normalizeInputAddress [_ input]
        (.normalizeInputAddress es6-loader input))

      (toModuleName [_ filename]
        (.toModuleName es6-loader filename))

      (toModuleIdentifier [_ filename]
        (.toModuleName es6-loader filename)))))

(defn ^Node get-root-node [ijs closure-compiler]
  (let [^CompilerInput input (->> (deps-proto/-source ijs)
                                  (js-source-file (:file ijs))
                                  (CompilerInput.))]
    (.getAstRoot input closure-compiler)))

(defn make-convert-js-module-options [opts]
  (-> opts
      (select-keys
        [:closure-warnings :closure-extra-annotations :pretty-print
         :language-in :language-out])
      (closure-options/set-options (CompilerOptions.))))

(defn get-source-files [lib-spec opts]
  (let [lib-filter (fn [lib-spec]
                     (map? lib-spec))
        libs (filter lib-filter (:libs opts))]
    (map #(js-source-file (:file %) (deps-proto/-source %))
         (mapcat #(get-all-lib-files % opts) libs))))

(defn ^com.google.javascript.jscomp.Compiler make-closure-compiler []
  (let [compiler (com.google.javascript.jscomp.Compiler.)]
    (com.google.javascript.jscomp.Compiler/setLoggingLevel Level/WARNING)
    compiler))

(defmulti make-preprocess-fn
  "Create a preprocessor for a given module type."
  (fn [lib-file-spec]
    (get-in lib-file-spec [:lib-spec :module-type])))

(defmethod make-preprocess-fn :commonjs [lib-file-spec]
  (let [opts (:opts lib-file-spec)
        externs []
        source-files (get-source-files lib-file-spec opts)
        closure-options (make-convert-js-module-options opts)
        closure-compiler (doto (make-closure-compiler)
                           (.init externs source-files closure-options))
        module-loader (make-cjs-loader lib-file-spec source-files)
        preprocessor (ProcessCommonJSModules. closure-compiler module-loader)]
    (fn [source]
      (let [^Node root (let [^CompilerInput input (->> source
                                                       (CompilerInput.))]
                         (.getAstRoot input closure-compiler))]
        (.process preprocessor nil root)
        (util/report-failure (.getResult closure-compiler))
        (.toSource closure-compiler root)))))

(defmethod make-preprocess-fn :default [lib-file-spec]
  (let [file (:file lib-file-spec)
        module-type (get-in lib-file-spec [:lib-spec :module-type])]
    (println "ERROR: don't know how to preprocess file \"" file "\" with module type " module-type)))

(defn preprocess-source [lib-file-spec]
  (let [url (:url lib-file-spec)
        source (:source lib-file-spec)
        working-dir (io/file (System/getProperty "user.dir"))
        file-relative-path (util/relativise working-dir url)
        preprocess! (make-preprocess-fn lib-file-spec)]
    (preprocess! (js-source-file file-relative-path source))))

;(defn preprocess-source (memoize preprocess-source'))

(defn preprocess-lib [{:keys [root] :as lib-spec} opts]
  (let [files (get-all-lib-files lib-spec opts)]
    (map (fn [lib-file-spec]
           (let [preprocessed-source (preprocess-source lib-file-spec)
                 parsed-ns (ns-util/parse-js-ns (string/split-lines preprocessed-source))]
             (map->PreprocessedJavaScriptFile (merge lib-file-spec {:provides (:provides parsed-ns)
                                                                    :requires (:requires parsed-ns)
                                                                    :source preprocessed-source}))))
         files)))