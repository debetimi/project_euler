(ns project-euler.p039)

;;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
;;; {20,48,52}, {24,45,51}, {30,40,50}
;;; For which value of p ≤ 1000, is the number of solutions maximised?

(defn triplets [n]
  (let [results (atom [])] 
    (doseq [c (range (int (inc (/ n 3))) (int (/ n 2)))]
      (doseq [b (range (int (inc (/ (- n c) 2))) c)]
        (let [a (- n c b)]
          (when (= (* c c) (+ (* a a) (* b b)))
            (swap! results conj [a b c])))))
    @results))

(defn solve []
  (let [max-len (fn [p x] (if (> (val x) (val p)) x p))
        nums (range 12 1001)]
    (reduce max-len (zipmap nums (map (comp count triplets) nums)))))
