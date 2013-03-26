(ns clojure-course-task02.core
  (:require [clojure.java.io :refer :all])
  (:gen-class))
   
(def join (comp flatten into))
(defn get-file-names [files] (map (memfn getName) files))
(defn get-dirs [files] (filter (memfn isDirectory) files))
            
(defn compare-file-names [file-name some-file]
  (re-find (re-pattern file-name) (.getName some-file)))

(defn get-suitable-names [suitable-file? files]
  (get-file-names (filter suitable-file? files)))

(defn parallel [func xs]
  (if (> (count xs) 1)
    (let [acc (ref [])]
      (dosync (alter acc conj @(future (func (first xs))))
              (alter acc conj @(future (func (second  xs)))))
      (conj @acc (map func (drop 2 xs))))
    (map func xs)))

(defn find-files-helper [suitable-file? dir]
  (let [files (.listFiles dir)]
    (join (parallel #(find-files-helper suitable-file? %) (get-dirs files))
          (get-suitable-names suitable-file? files))))
            
(defn find-files [file-name path]
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
