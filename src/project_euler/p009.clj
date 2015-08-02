(ns project-euler.p009)

;;; Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;;; a2 + b2 = c2
;;; For example, 32 + 42 = 9 + 16 = 25 = 52.
;;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;;; Find the product abc.

(defn solve
  []
  (let [result (atom [])] 
    (doseq [a (range 1 333)]
      (doseq [b (range a 500)]
        (let [c (- 1000 a b)]
          (when (= (* c c) (+ (* a a) (* b b)))
            (swap! result conj (* a b c))))))
    @result))
