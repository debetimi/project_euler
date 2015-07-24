(ns project-euler.p014
  (:require [clojure.math.numeric-tower :as math])) 

;;; The following iterative sequence is defined for the set of positive integers:
;;; n → n/2  (n is even)
;;; n → 3n + 1  (n is odd)
;;; Using the rule above and starting with 13, we generate the following sequence:
;;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;;; It can be seen that this sequence  (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet  (Collatz Problem), it is thought that all starting numbers finish at 1.
;;; Which starting number, under one million, produces the longest chain?
;; NOTE: Once the chain starts the terms are allowed to go above one million.

(def cache (atom {}))

(defn len-collatz
  "Calculates the length of a collatz series starting at n"
  [n]
  (if (contains? @cache n) 
    (get @cache n)
    (let [v (cond 
              (= 1 n) 1
              (even? n) (+ 1 (len-collatz (/ n 2)))
              :else (+ 2 (len-collatz (/ (+ 1 (* 3 n)) 2))))]
      (swap! cache assoc n v)
      v))) 

(defn longest-collatz
  "Returns number that produces longest collatz series in the range [1 max-seed]"
  [max-seed]
  (let [max-len (atom -1)
        max-n (atom -1)]
    (doseq [n (range 1 (inc max-seed))]
      (let [len (len-collatz n)]
        (when (> len @max-len) 
          (reset! max-len len)
          (reset! max-n n))))
    @max-n)) 
