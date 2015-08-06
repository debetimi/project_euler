(ns project-euler.p027
  (:require [project-euler.p003 :as p003]
            [project-euler.p007 :as p007]))

;;; Euler discovered the remarkable quadratic formula:
;;; n² + n + 41
;;; It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
;;; The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
;;; Considering quadratics of the form:
;;; n² + an + b, where |a| < 1000 and |b| < 1000
;;; where |n| is the modulus/absolute value of n
;;; e.g. |11| = 11 and |−4| = 4
;;; Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

(defn solve []
  (let [most-primes (atom -1)
        best-pair (atom nil)
        prime? (fn [x] (= 1 (count (p003/prime-factors x))))
        primes (take-while (partial > 1001) (p007/lazy-primes3))]
    (doseq [b primes]
      (doseq [a (range (- b) 1001 2)]
        (let [f (fn [n] (+' (*' n n) (*' a n) b))
              num-primes (count (take-while prime? (map f (range))))]
          (when (> num-primes @most-primes)
            (reset! most-primes num-primes)
            (reset! best-pair [a b])))))
    (reduce * @best-pair))) 
