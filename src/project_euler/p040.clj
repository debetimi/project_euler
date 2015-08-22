(ns project-euler.p040
  (:require [project-euler.utils :refer [nths num->digits]]
            [clojure.math.numeric-tower :as math]))

;An irrational decimal fraction is created by concatenating the positive integers:
;0.123456789101112131415161718192021...
;It can be seen that the 12th digit of the fractional part is 1.
;If dn represents the nth digit of the fractional part, find the value of the following expression.
;d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

;; TODO - write function called nths that takes a collection of indexes to retur
;; and iterates once


(defn solve []
  (let [indexes (map (comp dec (partial math/expt 10)) (range))]
    (reduce * (nths (flatten (map (comp num->digits inc) (range))) (take 7 indexes)))))
