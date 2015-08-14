(ns project-euler.p030
  (:require [clojure.math.numeric-tower :as math]))

;;; Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
;;; 1634 = 14 + 64 + 34 + 44
;;; 8208 = 84 + 24 + 04 + 84
;;; 9474 = 94 + 44 + 74 + 44
;;; As 1 = 14 is not a sum it is not included.
;;; The sum of these numbers is 1634 + 8208 + 9474 = 19316.
;;; Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


(defn sum-of-digits-to-the-n
  "Returns a set of numbers that are equal to the sum
   of their digits raised to the nth power"
  [n]
  (let [lim (* n (math/expt 9 n))
        num->sum (fn [x]
                   (->> x
                        str
                        seq 
                        (map (comp #(math/expt % n) read-string str))
                        (reduce +)))
        sum? (fn [x] (= x (num->sum x)))]
    (filter sum? (range 2 (inc lim)))))

(defn solve [] (reduce + (sum-of-digits-to-the-n 5)))
