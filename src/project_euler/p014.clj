(ns project-euler.p014
  (:require [clojure.math.numeric-tower :as math])) 


(def cache (atom {1 1}))

(defn len-collatz
  "Calculates the length of a collatz series starting at n"
  [n]
  (if (contains? @cache n) 
    (get @cache n)
    (let [v (cond 
              (= 1 n) n
              (even? n) (+ 1 (len-collatz (/ n 2)))
              :else (+ 1 (len-collatz (+ 1 (* 3 n)))))]
      (swap! cache assoc n v)
      v))) 

(defn longest-collatz
  "Returns number that produces longest collatz series up to max-seed"
  [max-seed]
  (let [max-len (atom -1)
        max-n (atom -1)]
    (dotimes [n max-seed]
      (let [n (inc n)
            len (len-collatz n)]
        (when (> len @max-len) 
          (reset! max-len len)
          (reset! max-n n))))
    @max-n)) 


(defn solve[]
  ;; Warm up solver to avoid stack over flow
  (doseq [n (range 1 5)]
    (longest-collatz (math/expt 10 n)))
  (longest-collatz 1e6))
