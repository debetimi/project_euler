(ns project-euler.p014
  (:require [clojure.math.numeric-tower :as math])) 

;;; The following iterative sequence is defined for the set of positive integers:
;;; n → n/2  (n is even)
;;; n → 3n + 1  (n is odd)
;;; Using the rule above and starting with 13, we generate the following sequence:
;;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;;; It can be seen that this sequence  (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet  (Collatz Problem), it is thought that all starting numbers finish at 1.
;;; Which starting number, under one million, produces the longest chain?
;; NOTE: Once the chain starts the terms are allowed to go above one million.

(defn len-collatz
  "Calculates the length of a collatz series starting at n"
  [n cache]
  (if-not (contains? @cache 1)
    (swap! cache assoc 1 1))
  (loop [i n acc 0]
    (if-let [v (get @cache i)]
      (get (swap! cache assoc n (+ acc v)) n)
      (if (even? i)
        (recur (/ i 2) (inc acc))
        (recur (/ (+ (* 3 i) 1) 2) (+ acc 2))))))

(defn longest-collatz
  "Returns seed and length of the longest collatz series in the range [1 max-seed]"
  [max-seed]
  (let [cache (atom {})]
    (dotimes [n max-seed]
      (len-collatz (inc n) cache))
    (apply max-key val @cache))) 
