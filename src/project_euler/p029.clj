(ns project-euler.p029
  (:require [project-euler.p003 :refer [prime-factors]]
            [clojure.math.numeric-tower :as m])) 

(defn log-n [n]
  (fn [x] (/ (Math/log x) (Math/log n))))


(defn solve [n] (- (* (- n 1) (- n 1)) (reduce + (map #((comp (fn [x] (quot x 2)) int) ((log-n %) (m/expt n n))) (range 2 (inc (/ n 2)))))))
;;(defn solve [n] (- (* (- n 1) (- n 1)) (reduce + (map #((comp (fn [x] (quot x 2)) int) ((log-n %) (m/expt n n))) (range 2 (inc (/ n 2)))))))
