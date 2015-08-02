(ns project-euler.p021
  (:require [project-euler.p003 :as p003]
            [clojure.math.numeric-tower :as m]))

;;; Let d(n) be defined as the sum of proper divisors of n  (numbers less than n which divide evenly into n).
;;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
;;; 
;;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;;; 
;;; Evaluate the sum of all the amicable numbers under 10000.

;; Creates factors by taking all list of prime factors
;; and reducing them into a list where each iteration of reduce
;; appends the current list to the current list multiplied by the factor
;; results in duplicates so have to apply set operation.
(def factors
  (memoize 
    (fn [number]
      (let [reducer (fn [factors prime] 
                      (concat factors (map (partial * prime) factors)))]
        (into (sorted-set) (reduce reducer [1] (p003/prime-factors number)))))))

;; Creates power sets by taking prime factorization 
;; and using the frequency to create a power set
;; then reducing the list of power sets by multiplying
;; each element in the powerset of a factor by the elements
;; in the list
(def factors2
  (memoize 
    (fn [n]
      (let [power-set (fn [prime-and-freq]
                        (let [prime (key prime-and-freq)
                              freq (val prime-and-freq)]
                          (map m/expt (repeat prime) (range 1 (inc freq)))))
            reducer (fn [factors prime-power-set] 
                      (concat factors (flatten (map #(map (partial * %) factors) prime-power-set))))]
        (reduce reducer [1] (map power-set (frequencies (p003/prime-factors n))))))))

(def d (memoize (fn [x] (reduce +' (conj (factors2 x) (- x))))))
(def amicable? (fn [a] (and (not= a (d a)) (= a (d (d a)))))) 

(defn solve 
  [] 
  (reduce + (filter amicable? (range 1e4))))
