(ns project-euler.p024)

;;; A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
;;; 012 021 102 120 201 210
;;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
(defn factorial [n] (reduce * (range 1 (inc n))))

(defn nth-lexicographic-permutation
  [word n]
  {:pre [(>= (factorial (count word)) n),
         (> n 0)]}
  (loop [options (vec (sort word)) lim n permutation ""]
    (if (empty? options) 
      permutation
      (let [len (count options)
            base (factorial (dec len))
            index (last (take-while #(> lim (* base %)) (range 10)))]
        (recur (vec (concat (subvec options 0 index) (subvec options (inc index) len))) (- lim (* index base)) (str permutation (nth options index)))))))

(defn solve [] (nth-lexicographic-permutation "0123456789" 1e6))
