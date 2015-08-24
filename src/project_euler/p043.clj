(ns project-euler.p043
  (:require [project-euler.utils :refer [digits->num enumerate any-pred]]
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

(defn leading-zero? [digits]
  (zero? (first digits)))

(defn zero-and-five? [digits]
  (let [f (set digits)]
    (boolean (and (f 0) (f 5)))))

(defn divisible? [n digits]
  (zero? (mod (digits->num (if (leading-zero? digits) (rest digits) digits)) n)))

(defn next-steps [pred n step]
  (let [remaining (remove (set step) (range 10))
        possibilities (map (partial conj step) remaining)]
    (filter (comp pred (partial drop n)) possibilities)))

(defn build-pandigital [prefixes [idx prime]]
  (let [divisible? (partial divisible? prime)]
    (apply concat (map (partial next-steps divisible? (inc idx)) prefixes))))

(defn solve []
  (let [options (remove (any-pred leading-zero? zero-and-five?) (apply concat (map permutations (combinations (range 10) 3))))]
    (reduce + (map digits->num (reduce build-pandigital options (enumerate [2 3 5 7 11 13 17]))))))
