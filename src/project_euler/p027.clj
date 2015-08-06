(ns project-euler.p027
  (:require [project-euler.p003 :as p003]
            [project-euler.p007 :as p007]
            [clojure.math.combinatorics :as combinatorics]))

(defn prime? [x] (= 1 (count (p003/prime-factors x))))

(defn quadratic [a b] (fn [n] (+' (*' n n) (*' a n) b)))

(defn solve []
  (let [most-primes (atom -1)
        best-pair (atom nil)
        primes (take-while (partial > 1001) (p007/lazy-primes3))]
    (doseq [a (range -999 1001 2)]
      (doseq [b primes]
        (let [f (quadratic a b)
              num-primes (count (take-while prime? (map f (range))))]
          (when (> num-primes @most-primes)
            (reset! most-primes num-primes)
            (reset! best-pair [a b])))))
    (reduce * @best-pair))) 
