(ns project-euler.p038
  (:require [project-euler.utils :as utils]
            [clojure.math.combinatorics :as combo]))

;;; Take the number 192 and multiply it by each of 1, 2, and 3:
;;; 192 × 1 = 192
;;; 192 × 2 = 384
;;; 192 × 3 = 576
;;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
;;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
;;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

(defn pandigital? [digits]
  (let [multiplier (utils/digits->num (take 4 digits))] ;; we know it has to be a 4 digit number since 2/3 digits would never produce exactly 9 digits 
    (loop [multiplicand 1 d digits]
      (if (empty? d) 
        (utils/digits->num digits)
        (let [prod (utils/num->digits (* multiplier multiplicand))
              len (count prod)]
          (if (= prod (take len  d))
            (recur (inc multiplicand) (drop len d))
            false))))))

(defn solve []
  (first (filter pandigital? (combo/permutations (reverse (range 1 10))))))
