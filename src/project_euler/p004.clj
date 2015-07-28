(ns project-euler.p004
  (:require [clojure.string :as s]))

;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defn palindrome? 
  [n]
  (let [as-string (str n)]
    (=  as-string (s/reverse as-string))))

;; inefficent but sufficient for this problem
(defn palindrome-products
  [start stop]
  (filter palindrome? (for [x (reverse (range start stop))
                            y (reverse (range start (inc x)))]
                        (* x y))))
(defn solve
  []
  (reduce max (palindrome-products 100 1000)))
