(ns project-euler.p021
  (:require [clojure.math.numeric-tower :as m]
            [project-euler.p003 :as p003]))

;;; Let d(n) be defined as the sum of proper divisors of n  (numbers less than n which divide evenly into n).
;;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
;;; 
;;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;;; 
;;; Evaluate the sum of all the amicable numbers under 10000.

(def factors
  (memoize 
    (fn [number]
      (let [prime-factors (p003/prime-factors number)
            reducer (fn [x y]
                      (concat x (map (partial * y) x)))]
        (into (sorted-set) (reduce reducer [1] prime-factors))))))

(def d (memoize (fn [x] (reduce +' (butlast (factors x))))))
(def amicable? (fn [a] (let [b (d a)
                             y (d b)]
                         (and (not= a b)(= a y))))) 
(def amicable-numbers (filter amicable? (range)))

(defn solve [] 
  (reduce + (take-while (partial > 10000) (filter amicable? (range)))))





