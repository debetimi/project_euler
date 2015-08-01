(ns project-euler.p010
 (require [project-euler.p007 :as p007])) 

(defn solve
  []
  (reduce +' (take-while (partial > 2e6) (p007/lazy-primes3))))
