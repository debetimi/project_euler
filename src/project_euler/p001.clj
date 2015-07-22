(ns project-euler.p001)
;;;  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
;;;  The sum of these multiples is 23.
;;;  Find the sum of all the multiples of 3 or 5 below 1000."

(defn evenly-spaced-series-sum
  "Returns the sum of evenly spaced series that starts at 0"
  [delta n]
  (*' (/ (+' delta (*' delta n)) 2) n))


(defn sum-multiples-of-3-and-5
  "Returns sums of multiples of three and five that are less n"
  [n]
  (let [n (dec n)]
    (long (+' (evenly-spaced-series-sum 3 (quot n 3)) 
              (evenly-spaced-series-sum 5 (quot n 5)) 
              (- (evenly-spaced-series-sum 15 (quot n 15)))))))

(defn -main
  "Main function for Hackerrank Code"
  []
  (doseq [value (map (comp sum-multiples-of-3-and-5 read-string) (rest (line-seq (clojure.java.io/reader *in*))))]
    (println value)))
