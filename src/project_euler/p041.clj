(ns project-euler.p041
  (:require [project-euler.utils :refer [num->digits prime? digits->num]]
            [clojure.math.combinatorics :refer [permutations]]))

;;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
;;; What is the largest n-digit pandigital prime that exists?

(defn solve []
  (let [mult3 (fn [digits] (zero? (rem (reduce + (range (inc digits))) 3))) ;if sum of digits is divisible by 3 then number is divisible by 3
        max-pandigital (fn [x] (first (filter prime? (remove even? (map digits->num (permutations (reverse (range 1 (inc x)))))))))]
    (some max-pandigital (remove mult3 (reverse (range 1 10))))))
