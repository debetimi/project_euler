(ns project-euler.p043
  (:require [project-euler.utils :refer [digits->num]]
            [clojure.math.combinatorics :refer [combinations permutations]]))

;;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
;;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
;;; d2d3d4=406 is divisible by 2
;;; d3d4d5=063 is divisible by 3
;;; d4d5d6=635 is divisible by 5
;;; d5d6d7=357 is divisible by 7
;;; d6d7d8=572 is divisible by 11
;;; d7d8d9=728 is divisible by 13
;;; d8d9d10=289 is divisible by 17
;;; Find the sum of all 0 to 9 pandigital numbers with this property.

(def primes [2 3 5 7 11 13 17])

(defn leading-zero?
  [digits]
  (zero? (first digits)))

(defn divisible-by-n? 
  [n digits]
  (zero? (mod (digits->num (if (leading-zero? digits) (rest digits) digits)) n)))

(defn append-valid-suffixes 
  [pred n prefix]
  (let [remaining (remove (set prefix) (range 10))
        valid-suffixes (map (partial conj prefix) remaining)]
    (filter (comp pred (partial drop n)) valid-suffixes)))

(defn reduce-fn 
  [prefixes [prime i]]
  (let [divisible? (partial divisible-by-n? prime)]
    (apply concat (map (partial append-valid-suffixes divisible? i) prefixes))))

(defn solve []
  (let [options (apply concat (map permutations (combinations (range 10) 3)))]
    (reduce + (map digits->num (reduce reduce-fn options (map vector primes (map inc (range))))))))
