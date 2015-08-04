(ns project-euler.p024)


(defn factorial 
  [n]
  (reduce * (range 1 (inc n))))

(defn nth-lexicographic-permutation
  [word n]
  {:pre [(>= (factorial (count word)) n),
         (> n 0)]}
  (loop [options (into [] (sort word)) lim n permutation "" j 0]
    (if (empty? options) 
      permutation
      (let [len (count options)
            base (factorial (dec len))
            index (last (take-while #(> lim (* base %)) (range 10)))
            ch (nth options index)]
        (recur (vec (concat (subvec options 0 index) (subvec options (inc index) len))) (- lim (* index base)) (str permutation ch) (inc j))))))

(defn solve
  []
  (nth-lexicographic-permutation "0123456789" 1e6))

