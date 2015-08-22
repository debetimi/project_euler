(ns project-euler.p042
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.string :refer [split]]
            [clojure.core.reducers :as r]))

;;; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
;;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;; By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
;;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?

(defn solve []
  (let [char->val (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (map inc (range)))
        word->val (fn [word] (r/fold + (r/map char->val word)))
        triangle? (fn [x] (integer? (* 1/2 (dec (sqrt (+ (* 8 x) 1)))))) ; n = 1/2 * (sqrt(8*x+1) - 1)
        words (slurp "resources/p042_words.txt")
        word-values (r/map (comp word->val read-string) (split words #","))
        reduce-fn (fn ([] 0) ([x _] (inc x)))]
    (r/fold + reduce-fn (r/filter triangle? word-values))))


