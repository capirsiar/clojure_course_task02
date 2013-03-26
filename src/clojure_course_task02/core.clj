(ns clojure-course-task02.core
  (:require [clojure.java.io :refer :all])
  (:require [clojure.core.reducers :as r])
  (:gen-class))
               
(defn compare-file-names [file-name some-file]
  (re-find (re-pattern file-name) (.getName some-file)))

(defn check-file-and-accumulate-dirs! [dirs suitable-file? some-file]
  "If some-file is a directory then add one in the collection of dirs and return false
   else return value of (suitable-file? some-file)"
  (if (.isDirectory some-file)
    (do (dosync (alter dirs conj some-file)) false)
    (suitable-file? some-file)))

(defn filter-files-and-accumulate-dirs! [dirs suitable-file? dir]
  (r/filter #(check-file-and-accumulate-dirs! dirs suitable-file? %)
            (.listFiles dir)))

(defn get-file-names [files]
  (into [] (r/map (memfn getName) files)))

(defn p-into-map [func xs]
  "similar to (reduce into (map func xs))"
  (loop [xs xs
         acc (ref [])]
    (cond (empty? xs) @acc  
          (= (count xs) 1) (into @acc (func (first xs)))
          :else (recur (drop 2 xs)
                       (do (dosync
                            (alter acc into @(future (func (first xs))))
                            (alter acc into @(future (func (second xs)))))
                           acc)))))

(defn find-files-helper [suitable-file? dir]
  (let [dirs (ref [])
        suitable-file-names (get-file-names 
                             (filter-files-and-accumulate-dirs! 
                              dirs suitable-file? dir))]
    (into (p-into-map #(find-files-helper suitable-file? %) @dirs)
          suitable-file-names)))
    
(defn find-files [file-name path]
  "Implement searching for a file using his name as a regexp."
  (find-files-helper #(compare-file-names file-name %) (file path)))

(defn usage []
  (println "Usage: $ run.sh file_name path"))

(defn -main [file-name path]
  (if (or (nil? file-name)
          (nil? path))
    (usage)
    (do
      (println "Searching for " file-name " in " path "...")
      (dorun (map println (find-files file-name path))))))
