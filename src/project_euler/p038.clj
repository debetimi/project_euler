(ns project-euler.p038
  (:require [project-euler.utils :refer [digits->num]]
            [clojure.math.combinatorics :as combo]))

;;; Take the number 192 and multiply it by each of 1, 2, and 3:
;;; 192 × 1 = 192
;;; 192 × 2 = 384
;;; 192 × 3 = 576
;;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
;;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
;;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

;; We know it has to be a 4 digit number since 2 
;; digits would produce number so 2 3 3 4 respectively
;; and 3 digits would produce numbers of 3 4 4 respectively
;; both of which aren't exactly 9 digits 
(defn pandigital-multiple? [digits] 
  (= (* 2 (digits->num (take 4 digits))) (digits->num (drop 4 digits))))

(defn solve []
  (digits->num (first (filter pandigital-multiple? (combo/permutations (reverse (range 1 10)))))))
