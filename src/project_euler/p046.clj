(ns project-euler.p046
  (:require [project-euler.utils :refer [lazy-primes prime?]]))

(defn goldbach? [n]
  (let [helper (fn [prime]
                 (some? (first (filter #(= n (+ prime (* 2 % %))) (range 1 n)))))]
    (some? (first (filter helper (take-while (partial >= n) lazy-primes))))))


(defn solve []
  (first (drop-while goldbach? (filter (every-pred odd? (complement prime?)) (map (partial + 2) (range))))))
