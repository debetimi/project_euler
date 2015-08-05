(ns project-euler.p003
  (:require [clojure.math.numeric-tower :as m]))

;;; https://projecteuler.net/problem=3
;;; The prime factors of 13195 are 5, 7, 13 and 29.

;;; What is the largest prime factor of the number 600851475143 ?

(defonce cache (atom {}))

(defn prime-factors
  [number]
  (loop [n number f 2 pfacts '()]
    (if (or (contains? @cache n) (<= n 1))
      (get (swap! cache assoc number (concat pfacts (get @cache n))) number) 
      (let [divisible? (zero? (rem n f))]
        (recur (if divisible? (/ n f) n) 
               (if divisible? f (if (= 2 f) (inc f) (+ 2 f))) 
               (if divisible? (conj pfacts f) pfacts))))))

(defn solve 
  []
  (first (prime-factors 600851475143)))
