(ns project-euler.p003)

;;; https://projecteuler.net/problem=3
;;; The prime factors of 13195 are 5, 7, 13 and 29.

;;; What is the largest prime factor of the number 600851475143 ?

(defn prime-factors
  [number]
  (loop [n number f 2 pfacts '()]
    (if (or (= n 0) (= 1 n)) 
      pfacts 
      (let [divisible? (zero? (rem n f))]
        (recur (if divisible? (/ n f) n) 
               (if divisible? f (inc f)) 
               (if divisible? (conj pfacts f) pfacts))))))

(defn solve 
  []
  (first (prime-factors 600851475143)))
