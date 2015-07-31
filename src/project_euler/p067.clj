(ns project-euler.p067
  (:require [project-euler.p018 :as p018]))

;; Same as problem 18 but with a provided text file

(def p67-pyramid (clojure.string/split (slurp "resources/triangle.txt") #"\n"))

(defn solve [] (p018/solve p67-pyramid))
