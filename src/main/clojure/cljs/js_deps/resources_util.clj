(ns cljs.js-deps.resources-util
  (:require [clojure.java.io :as io]
            [cljs.util :as util])
  (:import [java.io File]
           [java.net URL URLClassLoader]
           [java.util.zip ZipFile ZipEntry]))

; taken from pomegranate/dynapath
; https://github.com/tobias/dynapath/blob/master/src/dynapath/util.clj
(defn- all-classpath-urls
  "Walks up the parentage chain for a ClassLoader, concatenating any URLs it retrieves.
If no ClassLoader is provided, RT/baseLoader is assumed."
  ([] (all-classpath-urls (clojure.lang.RT/baseLoader)))
  ([cl]
   (->> (iterate #(.getParent ^ClassLoader %) cl)
        (take-while identity)
        reverse
        (filter (partial instance? URLClassLoader))
        (mapcat #(.getURLs ^URLClassLoader %))
        distinct)))

(defn ^ZipFile zip-file [jar-path]
  (cond
    (instance? File jar-path) (ZipFile. ^File jar-path)
    (string? jar-path) (ZipFile. ^String jar-path)
    :else
    (throw
      (IllegalArgumentException. (str "Cannot construct zipfile from " jar-path)))))

(defn jar-entry-names* [jar-path]
  (with-open [z (zip-file jar-path)]
    (doall (map #(.getName ^ZipEntry %) (enumeration-seq (.entries ^ZipFile z))))))

(def jar-entry-names (memoize jar-entry-names*))

(defn find-files-jar
  "Returns a seq of URLs of all resources in the given jar (optionally matching a predicate)"
  ([jar-path lib-path]
   (find-files-jar jar-path lib-path (constantly true)))
  ([jar-path lib-path predicate]
   (let [filter-fn #(and
                     (predicate %)
                     (.startsWith ^String % lib-path))]
     (map io/resource
          (filter filter-fn
                  (jar-entry-names jar-path))))))

(defn find-js-jar
  "Returns a seq of URLs of all JavaScript resources in the given jar"
  [jar-path lib-path]
  (find-files-jar jar-path lib-path #(.endsWith ^String % ".js")))

(defn find-files-fs
  "Finds resources from a path on the files system (optionally matching a predicate)"
  ([path]
   (find-files-fs path (constantly true)))
  ([path predicate]
   (let [file (io/file path)
         filter-fn #(and (.isFile ^File %)
                         (predicate (.getName ^File %)))]
     (when (.exists file)
       (map util/to-url
            (filter filter-fn
                    (file-seq file)))))))

(defn find-js-fs
  "Finds js resources from a path on the files system"
  [path]
  (find-files-fs path #(.endsWith % ".js")))

(defn find-files-classpath
  "Returns a seq of URLs of all files on the classpath (optionally matching a predicate)."
  ([path] (find-files-classpath path (constantly true)))
  ([path predicate]
   (->> (all-classpath-urls)
        (map io/file)
        (reduce
          (fn [files jar-or-dir]
            (let [name (.toLowerCase (.getName ^File jar-or-dir))
                  ext  (.substring name (inc (.lastIndexOf name ".")))]
              (->> (when (.exists ^File jar-or-dir)
                     (cond
                       (.isDirectory ^File jar-or-dir)
                       (find-files-fs (str (.getAbsolutePath ^File jar-or-dir) "/" path) predicate)

                       (#{"jar" "zip"} ext)
                       (find-files-jar jar-or-dir path predicate)

                       :else nil))
                   (remove nil?)
                   (into files))))
          []))))

(defn find-js-classpath
  "Returns a seq of URLs of all JavaScript files on the classpath."
  [path]
  (find-files-classpath path #(.endsWith % ".js")))

(defn find-resources
  "Returns a seq of URLs to all resources on the classpath or within
   a given (directory) path on the filesystem (optionally matching a predicate).
   [path] only applies to the latter case."
  ([path] (find-resources path (constantly true)))
  ([path predicate]
   (let [file (io/file path)]
     (if (.exists file)
       (find-files-fs path predicate)
       (find-files-classpath path predicate)))))

(defn find-js-resources [path]
  "Returns a seq of URLs to all JavaScript resources on the classpath or within
a given (directory) path on the filesystem. [path] only applies to the latter
case."
  (find-resources path #(.endsWith % ".js")))