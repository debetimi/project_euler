(ns project-euler.p016
  (:require [clojure.math.numeric-tower :as math]))  

;; 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;; What is the sum of the digits of the number 21000?

(defn power-digit-sum
  [a n]
 (let [value (bigint (math/expt a  n))]
   (apply + (map (comp read-string str) (str value))))) 

(defn- solve
  []
  (power-digit-sum 2 1000))
