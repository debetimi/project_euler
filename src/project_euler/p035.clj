(ns project-euler.p035
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as string]
            [project-euler.p007 :refer [lazy-primes3]]
            [project-euler.p003 :refer [prime-factors]]))

;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
;; How many circular primes are there below one million?
(defn prime? [x] (= x (first (prime-factors x))))

(defn rotate [w] (str (subs w 1) (subs w 0 1)))

(defn circular? [prime]
  (every? prime? (map read-string (rest (take (count (str prime)) (iterate rotate (str prime)))))))

(defn only-odd-digits? [x] (not-any? even? (map (comp read-string str) (str x))))

(defn solve 
  "n must be a power of 10"
  ([] (solve 1e6))
  ([n] (inc (count (filter circular? (filter only-odd-digits? (take-while (partial > n) (lazy-primes3))))))))
