(ns project-euler.p015)

;;; https://projecteuler.net/problem=15 
;;; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
;;; How many such routes are there through a 20×20 grid?

(defn lattice-paths
  "Finds the number of lattice paths for the given dimensions"
  [dim]
  (let [paths (atom {})]
    (dotimes [y (inc (second dim))] 
      (dotimes [x (inc (first dim))] 
        (let [coords [x y]
              weight (cond
                       (= [0 0] coords) 1N
                       :else (+ (get @paths [(dec x) y] 0) (get @paths [x (dec y)] 0)))]
          (swap! paths assoc coords weight))))
    (get @paths dim)))

(defn- solve
  []
  (lattice-paths [20 20])) 
