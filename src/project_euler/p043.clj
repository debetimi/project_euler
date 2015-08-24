(ns project-euler.p043
  (:require [project-euler.utils :refer [digits->num]]
            [clojure.math.combinatorics :refer [combinations permutations]]))

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
