(ns cljs.preprocess
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.google.javascript.jscomp CompilerOptions SourceFile
                                         Result JSError ProcessCommonJSModules
                                         ES6ModuleLoader TransformAMDToCJSModule
                                         ProcessEs6Modules CompilerInput IJavascriptModuleLoader]
           [com.google.javascript.rhino Node]
           (java.io File)
           (java.net URI)
           (java.util List)))

#_(util/compile-if is-new-es6-loader?
  (defn make-es6-loader [source-files libs]
    (let [^List module-roots (list default-module-root)
          ^List compiler-inputs (map #(CompilerInput. %) source-files)]
      (ES6ModuleLoader. module-roots compiler-inputs)))
  (defn make-es6-loader [closure-compiler file]
    (let [module-root (get-js-module-root file)]
      (ES6ModuleLoader. closure-compiler module-root))))

(defn relative-path? [path]
  (if [path (.toString path)]
    (or (.startsWith path "../")
        (.startsWith path "./"))))

(defprotocol ILookupMap
  (lookup [this what])
  (inverted-lookup [this what]))

(defn join-path [path-elements]
  (string/join File/separator path-elements))

(defn parent-dir [path]
  (let [idx (.lastIndexOf path "/")]
    (if (> idx 0)
      (subs path 0 idx)
      path)))

(defn make-cjs-loader [lib-spec source-files all-lib-specs]
  (let [module-roots (apply list (into #{} (filter some? (map (comp parent-dir :path) all-lib-specs))))
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

#_(defn make-cjs-loader [lib-spec compiler-inputs all-lib-specs]
  (let [module-roots (into #{} (map identity all lib-specs))
        es6-loader (ES6ModuleLoader. module-roots compiler-inputs)
        lookup-map (lookup-map-for-lib-spec lib-spec)
        inverted-lookup-map (invert-lookup-map lookup-map)
        ]
    (reify
      IJavascriptModuleLoader
      (locateCommonJsModule [_ require-name context]
        (let [lookup-value (if (relative-path? require-name)
                             (let [context-name (.getName context)
                                   cwd (.toString (.getAbsolutePath (io/file "")))
                                   context-uri (URI. (str "file:" cwd "/" context-name))
                                   resolved (.resolve context-uri require-name)]
                               ;(println "REQUIRE-NAME=" require-name " CONTEXT-URI=" context-uri " RESOLVED=" resolved)
                               resolved)
                             require-name)
              lookup-value (.getPath lookup-value)]
          ; (println "  A=" (URI. (str "file:" lookup-value)) " RESULT="  (get inverted-lookup-map (URI. (str "file:" lookup-value))))
          ; (println "  B=" (URI. (str "file:" (make-path [lookup-value "index.js"]))) " RESULT="  (get inverted-lookup-map (URI. (str "file:" (make-path [lookup-value "index.js"])))))
          ; (println "  C=" (URI. (str "file:" (str lookup-value ".js"))) " RESULT=" (get inverted-lookup-map (URI. (str "file:" (str lookup-value ".js")))))
          (when-let [lookup-result (or (get inverted-lookup-map (URI. (str "file:" lookup-value)))
                                       (get inverted-lookup-map (URI. (str "file:" (join-path [lookup-value "index.js"]))))
                                       (get inverted-lookup-map (URI. (str "file:" (str lookup-value ".js")))))]
            (let [lookup-result (first lookup-result)
                  ;lookup-result (.toURI (get-in lookup-map [lookup-result :url]))]
                  ]
              ;(println "REQUIRE-NAME=" require-name " LOOKUP-RESULT=" lookup-result)
              (URI. lookup-result)))))

      (locateEs6Module [_ module-name context]
        (.locateEs6Module es6-loader module-name context))

      (normalizeInputAddress [_ input]
        (let [input-name      (.getName input)
              ; _ (println "INPUT-NAME=" input-name)
              cwd (.toString (.getAbsolutePath (io/file "")))
              input-uri (URI. (str "file:" cwd "/" input-name))
              ; _ (println "INPUT-URI=" input-uri)
              canonical-name  (first (get inverted-lookup-map input-uri))]
          ; (println "CANONICAL-NAME=" canonical-name)
          (or (URI. canonical-name)
              input)))

      (toModuleName [_ filename]
        (let [filename (.toString filename)]
          ; (println "TO MODULE NAME CALLED; FILENAME=" filename " RESULT=" (get-in lookup-map [filename :module-name]))
          (get-in lookup-map [filename :module-name])))

      (toModuleIdentifier [_ filename]
        (let [filename (.toString filename)]
          ; (println "TO MODULE IDENTIFIER CALLED; FILENAME=" filename " RESULT=" (module-name-to-identifier (get-in lookup-map [filename :module-name])))
          (module-name-to-identifier (get-in lookup-map [filename :module-name]))))

      ILookupMap
      (lookup [_ what]
        ; (pp/pprint lookup-map)
        (get lookup-map what))

      (inverted-lookup [_ what]
        (get inverted-lookup-map what)))))