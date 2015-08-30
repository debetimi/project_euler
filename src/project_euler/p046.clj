(ns project-euler.p046
  (:require [project-euler.utils :refer [lazy-primes prime?]]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn goldbach? [n]
  (some? (first (filter #(integer? (sqrt (/ (- n %) 2))) (take-while (partial >= n) lazy-primes)))))

(defn solve []
  (first (drop-while goldbach? (filter (every-pred odd? (complement prime?)) (map #(+ 33 (* 2 %)) (range))))))
