(ns project-euler.p033
  (:require [clojure.string :as string]))

;;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
;;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
;;; There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
;;; If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

(defn curious?
  [n d]
  (let [frac (/ n d)
        tens-n (quot n 10)
        singles-n (rem n 10)
        tens-d (quot d 10)
        singles-d (rem d 10)]
    (or (and (= singles-n tens-d)
             (= frac (/ tens-n singles-d)))
        (and (= tens-n singles-d)
             (= frac (/ singles-n tens-d)))
        (and (= singles-n singles-d)
             (= frac (/ tens-n tens-d)))
        (and (= tens-n tens-d)
             (= frac (/ singles-n singles-d))))))

(defn potential-denominators
  "Returns a collection of potentially curious denominators for numerator"
  [n]
  (let [t (quot n 10)
        s (rem n 10)]
    (into #{}  (filter (partial < n)
                       (concat (map (partial + (* t 10)) (range s 10))
                               (map (partial + (* s 10)) (range 1 10))
                               (map (partial + t) (range (* (inc t) 10) 100 10))
                               (map (partial + s) (range 10 100 10)))))))

(defn solve [] 
  (let [curious-fractions (atom [])
        mult10 (fn [x] (zero? (rem x 10)))]
    (doseq [n (remove mult10 (range 11 100))]
      (doseq [d (potential-denominators n)]
        (when (curious? n d) (swap! curious-fractions conj (/ n d)))))
    (denominator (reduce * @curious-fractions))))
