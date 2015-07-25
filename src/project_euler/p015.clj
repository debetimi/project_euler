(ns project-euler.p015)

;;; https://projecteuler.net/problem=15 
;;; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
;;; How many such routes are there through a 20×20 grid?

(defn lattice-paths
  "Finds the number of lattice paths"
  ([] (lattice-paths [20 20]))
  ([dim]
   (let [paths (atom {})]
     (dotimes [i (inc (first dim))] 
       (dotimes [j (inc (second dim))] 
         (let [weight (cond
                        (= [0 0] [i j]) 1N
                        :else (+ (get @paths [(dec i) j] 0) (get @paths [i (dec j)] 0)))]
           (swap! paths assoc [i j] weight))))
     (get @paths dim))))
