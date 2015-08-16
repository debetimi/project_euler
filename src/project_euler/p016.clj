(ns project-euler.p016
  (:require [clojure.math.numeric-tower :as math]
            [project-euler.utils :as utils]))  

;; 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;; What is the sum of the digits of the number 21000?

(defn power-digit-sum [a n] (reduce + (utils/num->digits (math/expt a n)))) 

(defn solve [] (power-digit-sum 2 1000))
