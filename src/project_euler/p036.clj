(ns project-euler.p036
  (require [clojure.string :as string]))

;;; The decimal number, 585 = 10010010012  (binary), is palindromic in both bases.
;;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
;;; (Please note that the palindromic number, in either base, may not include leading zeros.)

(defn solve []
  (letfn [(palindrome? [x]
            (= x (string/reverse x)))
          (double-base-palindrome? [x]
            (and (palindrome? (Integer/toString x 2)) (palindrome? (Integer/toString x 10))))]
    (reduce +' (filter double-base-palindrome? (range 1e6)))))
