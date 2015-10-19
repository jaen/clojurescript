(ns cljs.js-deps
  (:use cljs.js-deps.protocols
        cljs.js-deps.resources-util)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.js-deps.ns-util :as ns-util]
            [cljs.preprocess :as preprocess])
  (:import [java.io File]
           [java.util.logging Level]
           [java.io File BufferedInputStream StringWriter]
           [java.net URL URI URLClassLoader]
           [java.util.zip ZipFile ZipEntry]))

(defn build-index
  "Index a list of dependencies by namespace and file name. There can
  be zero or more namespaces provided per file. Upstream foreign libraies
  will have their options merged with local foreign libraries to support
  fine-grained overriding."
  [deps]
  (reduce
    (fn [index dep]
      (let [provides (:provides dep)
            index'   (if (seq provides)
                       (reduce
                         (fn [index' provide]
                           (if (:foreign dep)
                             (update-in index' [provide] merge dep)
                             (assoc index' provide dep)))
                         index provides)
                       index)]
        (if (:foreign dep)
          (update-in index' [(:file dep)] merge dep)
          (assoc index' (:file dep) dep))))
    {} deps))

(defn dependency-order-visit
  [state ns-name]
  (let [file (get state ns-name)]
    (if (or (:visited file) (nil? file))
      state
      (let [state (assoc-in state [ns-name :visited] true)
            deps (:requires file)
            state (reduce dependency-order-visit state deps)]
        (assoc state :order (conj (:order state) file))))))

(defn- pack-string [s]
  (if (string? s)
    {:provides (-provides s)
     :requires (-requires s)
     :file (str "from_source_" (gensym) ".clj")
     ::original s}
    s))

(defn- unpack-string [m]
  (or (::original m) m))

(defn dependency-order
  "Topologically sort a collection of dependencies."
  [coll]
  (let [state (build-index (map pack-string coll))]
    (map unpack-string
         (distinct
          (:order (reduce dependency-order-visit (assoc state :order []) (keys state)))))))


;; Dependencies
;; ============
;;
;; Find all dependencies from files on the classpath. Eliminates the
;; need for closurebuilder. cljs dependencies will be compiled as
;; needed.

(defn find-url
  "Given a string, returns a URL. Attempts to resolve as a classpath-relative
  path, then as a path relative to the working directory or a URL string"
  [path-or-url]
  (or (io/resource path-or-url)
      (try (io/as-url path-or-url)
           (catch java.net.MalformedURLException e
             false))
      (io/as-url (io/as-file path-or-url))))

(defn load-foreign-library*
  "Given a library spec (a map containing the keys :file
  and :provides), returns a map containing :provides, :requires, :file
  and :url"
  ([lib-spec] (load-foreign-library* lib-spec false))
  ([lib-spec cp-only?]
    (let [find-func (if cp-only? io/resource find-url)]
      (cond->
        (merge lib-spec
          {:foreign true
           :url     (find-func (:file lib-spec))})
        (:file-min lib-spec)
        (assoc :url-min (find-func (:file-min lib-spec)))))))

(def load-foreign-library (memoize load-foreign-library*))

(defn- library-graph-node
  "Returns a map of :provides, :requires, and :url given a URL to a goog-style
JavaScript library containing provide/require 'declarations'."
  ([url] (library-graph-node url nil))
  ([url lib-path]
   (with-open [reader (io/reader url)]
     (-> reader line-seq ns-util/parse-js-ns
         (merge
         {:url url}
         (when lib-path
           {:closure-lib true :lib-path lib-path}))))))

(defmulti load-library*
          (fn [spec opts]
            (cond
              (string? spec) :path
              (map? spec)    :spec)))

(defmethod load-library* :path [path opts]
  (->> (find-js-resources path)
       (map #(library-graph-node % path))
       (filter #(seq (:provides %)))))

(defmethod load-library* :spec [lib-spec opts]
  (preprocess/preprocess-lib lib-spec opts))

(def load-library (memoize load-library*))

(defn lib-exists [lib-spec]
  (cond
    (string? lib-spec) (.exists (io/file lib-spec))
    (map? lib-spec)    (.exists (io/file (:root lib-spec)))))

(defn library-dependencies
  [{libs :libs foreign-libs :foreign-libs
    ups-libs :ups-libs ups-flibs :ups-foreign-libs
    :as opts}]
  (concat
    (mapcat #(load-library % opts) ups-libs) ;upstream deps
    ; :libs are constrained to filesystem-only at this point; see
    ; `find-classpath-lib` for goog-style JS library lookup
    (mapcat #(load-library % opts) (filter lib-exists libs))
    (map #(load-foreign-library % true) ups-flibs) ;upstream deps
    (map load-foreign-library foreign-libs)))

(comment
  ;; load one library
  (load-library* "closure/library/third_party/closure")
  ;; load all library dependencies
  (library-dependencies {:libs ["closure/library/third_party/closure"]})
  (library-dependencies {:foreign-libs [{:file "http://example.com/remote.js"
                                          :provides ["my.example"]}]})
  (library-dependencies {:foreign-libs [{:file "local/file.js"
                                            :provides ["my.example"]}]})
  (library-dependencies {:foreign-libs [{:file "cljs/nodejs_externs.js"
                                          :provides ["my.example"]}]}))

;; NO LONGER NEEDED, deps.js and base.js now removed from build
;(defn goog-resource
;  "Helper to disambiguate Google Closure Library resources from Google
;   Closure Library Third Party resoures."
;  [path]
;  (first
;    (filter
;      (fn [res]
;        (re-find #"(\/google-closure-library-0.0*|\/google-closure-library\/)" (.getPath ^URL res)))
;      (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread)) path)))))

(defn goog-dependencies*
  "Create an index of Google dependencies by namespace and file name."
  []
  (letfn [(parse-list [s] (when (> (count s) 0)
                            (-> (.substring ^String s 1 (dec (count s)))
                                (string/split #"'\s*,\s*'"))))]
    (with-open [reader (io/reader (io/resource "goog/deps.js"))]
      (->> (line-seq reader)
           (map #(re-matches #"^goog\.addDependency\(['\"](.*)['\"],\s*\[(.*)\],\s*\[(.*)\],.*\);.*" %))
           (remove nil?)
           (map #(drop 1 %))
           (remove #(.startsWith ^String (first %) "../../third_party"))
           (map #(hash-map :file (str "goog/" (nth % 0))
                           :provides (parse-list (nth % 1))
                           :requires (parse-list (nth % 2))
                           :group :goog))
           (doall)))))

(def goog-dependencies (memoize goog-dependencies*))

(defn js-dependency-index
  "Returns the index for all JavaScript dependencies. Lookup by
  namespace or file name."
  [opts]
  ; (library-dependencies) will find all of the same libs returned by
  ; (goog-dependencies), but the latter returns some additional/different
  ; information (:file instead of :url, :group), so they're folded in last to
  ; take precedence in the returned index.  It is likely that
  ; (goog-dependencies), special-casing of them, goog/deps.js, etc can be
  ; removed entirely, but verifying that can be a fight for another day.
  (build-index (concat (library-dependencies opts) (goog-dependencies))))

(defn find-classpath-lib
  "Given [lib], a string or symbol naming a goog-style JavaScript library
  (i.e. one that uses goog.provide and goog.require), look for a resource on the
  classpath corresponding to [lib] and return a map via `library-graph-node`
  that contains its relevant metadata.  The library found on the classpath
  _must_ contain a `goog.provide` that matches [lib], or this fn will return nil
  and print a warning."
  [lib]
  (when-let [lib-resource (some-> (name lib)
                            (.replace \. \/)
                            (.replace \- \_)
                            (str ".js")
                            io/resource)]
    (let [{:keys [provides] :as lib-info} (library-graph-node lib-resource)]
      (if (some #{(name lib)} provides)
        (assoc lib-info :closure-lib true)
        (binding [*out* *err*]
          (println
            (format
              (str "WARNING: JavaScript file found on classpath for library `%s`, "
                   "but does not contain a corresponding `goog.provide` declaration: %s")
              lib lib-resource)))))))
