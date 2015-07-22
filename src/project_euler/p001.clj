(ns project-euler.p001)

(defn mult 
  "Returns the multiplicity of n for a given divisor"
  [n divisor]
  (long (Math/floor (/ n divisor))))

(defn evenly-spaced-series-sum
  "Returns the sum of evenly spaced series that starts at 0"
  [spacing n]
  (*' (/ (+' spacing (*' spacing n)) 2) n))


(defn sum-multiples-of-3-and-5
  "If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
   The sum of these multiples is 23.
   Find the sum of all the multiples of 3 or 5 below 1000."
  [n]
  (let [n (dec n)
        mult3 (mult n 3) 
        mult5 (mult n 5) 
        mult15 (mult n 15)]
    (long (+' (evenly-spaced-series-sum 3 mult3) 
              (evenly-spaced-series-sum 5 mult5) 
              (- (evenly-spaced-series-sum 15 mult15))))))

(defn -main
  "Main function for Hackerrank Code"
  []
  (doseq [value (map (comp sum-multiples-of-3-and-5 read-string) (rest (line-seq (clojure.java.io/reader *in*))))]
    (println value)))
