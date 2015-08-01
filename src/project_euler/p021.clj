(ns project-euler.p021
  (:require [project-euler.p003 :as p003]))

;;; Let d(n) be defined as the sum of proper divisors of n  (numbers less than n which divide evenly into n).
;;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
;;; 
;;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;;; 
;;; Evaluate the sum of all the amicable numbers under 10000.

(def factors
  (memoize 
    (fn [number]
      (let [reducer (fn [x y] (concat x (map (partial * y) x)))]
        (into (sorted-set) (reduce reducer [1] (p003/prime-factors number)))))))
(def d (memoize (fn [x] (reduce +' (conj (factors x) (- x))))))
(def amicable? (fn [a] (and (not= a (d a)) (= a (d (d a)))))) 

(defn solve [] (reduce + (filter amicable? (range 10000))))
