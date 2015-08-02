(ns project-euler.p003
  (:require [clojure.math.numeric-tower :as m]))

;;; https://projecteuler.net/problem=3
;;; The prime factors of 13195 are 5, 7, 13 and 29.

;;; What is the largest prime factor of the number 600851475143 ?

(defonce cache (atom {}))

(defn prime-factors
  [number]
  (loop [n number f 2 pfacts '() terminate (bigint (m/ceil (m/sqrt number)))]
    (cond 
      (or (contains? @cache n) (= n 0) (= 1 n)) (get (swap! cache assoc number (concat pfacts (get @cache n))) number) 
      (and (> f terminate) (empty? pfacts)) (get (swap! cache assoc number [number]) number) 
      :else (let [divisible? (zero? (rem n f))]
              (recur (if divisible? (/ n f) n) 
                     (if divisible? f (if (= 2 f) (inc f) (+ 2 f))) 
                     (if divisible? (conj pfacts f) pfacts) terminate)))))

(defn solve 
  []
  (first (prime-factors 600851475143)))
