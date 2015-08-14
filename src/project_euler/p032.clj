(ns project-euler.p032
  (:require  [clojure.math.combinatorics :as combo]))

;;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
;;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;;; HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


(defn solve [] 
  (let [all-digits (seq "123456789") 
        pandigitals (atom #{})]
    (doseq [i (range 2000)]
      (doseq [j (range i 2000)]
        (let [product (* i j)
              digits-used (sort (str i j product))]
          (when (= all-digits digits-used)
            (swap! pandigitals conj product)))))
    (reduce + @pandigitals)))
