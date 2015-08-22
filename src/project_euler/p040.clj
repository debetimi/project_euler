(ns project-euler.p040
  (:require [project-euler.utils :as utils]
            [clojure.math.numeric-tower :as math]))

;An irrational decimal fraction is created by concatenating the positive integers:
;0.123456789101112131415161718192021...
;It can be seen that the 12th digit of the fractional part is 1.
;If dn represents the nth digit of the fractional part, find the value of the following expression.
;d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

;; TODO - write function called nths that takes a collection of indexes to retur
;; and iterates once

(defn nths [coll indices]
  {:pre [(every? (partial <= 0) indices)]}
  (let [sorted (into [] (apply sorted-set indices))
        terminal (last sorted)
        reducer (fn [[collected remaining prev-index] index]
                  (let [delta (- index prev-index)
                        next-val (last (take delta remaining))]
                    (if (= terminal index)
                      (reduced (conj collected next-val))
                      [(conj collected next-val) (drop delta remaining) index])))]
    (reduce reducer [[] coll -1] sorted)))

(defn solve []
  (let [indexes (map (comp dec (partial math/expt 10)) (range))]
    (reduce * (map (partial nth (flatten (map (comp utils/num->digits inc) (range)))) (take 7 indexes)))))
