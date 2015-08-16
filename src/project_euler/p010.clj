(ns project-euler.p010
 (require [project-euler.utils :refer [lazy-primes]])) 

;;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;; Find the sum of all the primes below two million.

(defn solve [] (reduce +' (take-while (partial > 2e6) lazy-primes)))
