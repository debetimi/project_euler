(ns project-euler.p005
  (:require [project-euler.utils :refer [lcm]]))

;;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn lcm-of-one-to-n [n] (reduce lcm (range 1 (inc n))))

(defn solve [] (lcm-of-one-to-n 20))
