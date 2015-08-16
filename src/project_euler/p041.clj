(ns project-euler.p041
  (:require [project-euler.utils :refer [num->digits prime?]]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.math.numeric-tower :refer [ceil]]))

;;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
;;; What is the largest n-digit pandigital prime that exists?

(defn solve []
  (let [mult3 (fn [digits] (zero? (rem (reduce + (range (inc digits))) 3))) ;; if sum of digits is divisible by 3 then number is divisible by 3
        pandigital? (fn [x] (= (range 1 (inc (ceil (Math/log10 x)))) (sort (num->digits x))))
        has-pandigitals? (fn [x] (filter prime? (remove even? (map (comp read-string (partial apply str)) (permutations (range 1 (inc x)))))))]
    (reduce max (some has-pandigitals? (reverse (remove mult3 (range 1 10)))))))
