(ns project-euler.p034
  (:require [project-euler.utils :refer [factorial num->digits]]))

;;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.
;;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(defn solve []
  (letfn [(curious? [n]
           (= n (reduce +' (map factorial (num->digits n)))))]
    (reduce + (filter curious? (range 3 1e6)))))  ;;; max value of n digit number is n*9! so limit for n is 10^(n-1) > n*9!, n ~=7
