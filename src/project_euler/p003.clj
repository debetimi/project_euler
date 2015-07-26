(ns project-euler.p003)

;;; https://projecteuler.net/problem=3
;;; The prime factors of 13195 are 5, 7, 13 and 29.

;;; What is the largest prime factor of the number 600851475143 ?

(defn prime-factors
  [number]
  (loop [n number f 2 pfacts '()]
    (if (= 1 n) 
     pfacts 
      (let [m (loop [i n]
                (if-not (zero? (rem i f)) 
                 i 
                  (recur (/ i f))))]
        (recur m (inc f) (if (= m n) 
                           pfacts 
                           (conj pfacts f)))))))

(defn- solve 
  []
  (first (prime-factors 600851475143)))
