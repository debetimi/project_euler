(ns project-euler.p042
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.string :refer [split]]
            [clojure.core.reducers :as r]))

(defn solve []
  (let [char->val (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (map inc (range)))
        word->val (fn [word] (r/fold + (r/map char->val word)))
        triangle? (fn [x] (integer? (* 1/2 (dec (sqrt (+ (* 8 x) 1))))))
        words (slurp "resources/p042_words.txt")
        word-values (r/map (comp word->val read-string) (split words #","))
        reduce-fn (fn ([] 0) ([x _] (inc x)))]
    (r/fold + reduce-fn (r/filter triangle? word-values))))


