(ns project-euler.p026
  (:require [project-euler.utils :refer [lazy-primes]]))

;;; A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
;;; 
;;; 1/2 =   0.5
;;; 1/3 =   0. (3)
;;; 1/4 =   0.25
;;; 1/5 =   0.2
;;; 1/6 =   0.1 (6)
;;; 1/7 =   0. (142857)
;;; 1/8 =   0.125
;;; 1/9 =   0. (1)
;;; 1/10  =   0.1
;;; Where 0.1 (6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
;;; 
;;; Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

;; https://en.wikipedia.org/wiki/Cyclic_number

(defn cyclic-for-prime
  [p] 
  (loop [t 1 r 1 n 0]
    (let [x (* r 10)
          d (bigint (/ x p))
          r (mod x p)
          n (+ d (* n 10))]
      (if (or (zero? r) (= r 1)) 
        (if (= t (dec p)) n nil)
        (recur (inc t) r n)))))

(defn solve []
  (let [primes (take-while (partial > 1000) lazy-primes)
        reducer (fn [x y] (if (> (+ (count (str (val x))) (quot (key x) 10)) (+ (count (str (val y))) (quot (key y) 10))) x y))]
    (reduce reducer (remove (comp nil? (partial val)) (zipmap primes (map cyclic-for-prime primes))))))
