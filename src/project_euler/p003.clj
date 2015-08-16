(ns project-euler.p003
  (:require [project-euler.utils :as utils]))

;;; https://projecteuler.net/problem=3
;;; The prime factors of 13195 are 5, 7, 13 and 29.
;;; What is the largest prime factor of the number 600851475143 ?

(defn solve [] (first (utils/prime-factors 600851475143)))
