(ns project-euler.p035
  (:require [project-euler.utils :as utils]))

;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
;; How many circular primes are there below one million?

(defn solve 
  ([] (solve 1e6))
  ([n] (letfn [(only-odd-digits? [x]
                 (every? odd? (utils/num->digits x)))
               (circular? [prime] 
                 (every? utils/prime? (map read-string (rest (take (count (str prime)) (iterate utils/left-shift (str prime)))))))]
         (inc (count (filter circular? (filter only-odd-digits? (take-while (partial > n) utils/lazy-primes))))))))
