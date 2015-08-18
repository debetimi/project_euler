(ns project-euler.p037
  (:require [project-euler.utils :as utils]))

;;; The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
;;; Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
;;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(defn solve []
  (let [drop-right-digit (fn [x] ((comp read-string (partial apply str) butlast str) x))
        drop-left-digit (fn [x] ((comp read-string (partial apply str) rest str) x))
        truncatable? (fn [x] (let [len (count (str x))]
                               (and (every? utils/prime? (rest (take len (iterate drop-right-digit x))))
                                    (every? utils/prime? (rest (take len (iterate drop-left-digit x)))))))]
    (reduce + (take 11 (filter truncatable? (drop 4 utils/lazy-primes))))))
