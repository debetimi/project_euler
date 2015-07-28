(ns project-euler.p017
  (:require [clojure.string :as s]))

(def singles {0 "" 1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine"})
(def teens {0 "ten" 1 "eleven" 2 "twelve" 3 "thirteen" 4 "fourteen" 5 "fifteen" 6 "sixteen" 7 "seventeen" 8 "eighteen" 9 "nineteen"})
(def tens {0 "" 1 "" 2 "twenty" 3 "thirty" 4 "forty" 5 "fifty" 6 "sixty" 7 "seventy" 8 "eighty" 9 "ninety"})

(def periods ["" "thousand" "million" "billion" "trillion"])
(def inner-suffixes ["hundred"])
(def inner-prefixes ["" "and" "and"])
(def order (cycle [singles tens]))


(defn- period-to-word
  "Converts a period sequence to a word
   Eg: (period-to-word '(1 0 1)) -> 'onehundredandone')"
  [period]
  (let [size (count period)
        words (take size (drop (dec size) order))
        suffixes (drop (- 3 size) inner-suffixes)] 
    (loop [digits (reverse (map (comp read-string str) period)) words words period-rep "" suffixes suffixes prefix ""] 
      (if (empty? digits) 
        period-rep
        (let [digit (first digits)
              word (get (first words) digit)
              suffix (if-not (empty? word) (str (first suffixes)) "")]
          (recur (rest digits) 
                 (if-not (and (not (zero? digit)) (empty? word))
                   (rest words) 
                   (list teens)) 
                 (str period-rep (if-not (empty? word) prefix "") word suffix)
                 (rest suffixes)
                 (if-not (empty? suffix) "and" (if-not (empty? word) "" prefix))))))))

(def period-to-word-cached (memoize period-to-word))

(defn num-to-word
  [number]
  (let [partitioned-number (partition 3 3 [] (s/reverse (str number)))
        add-suffix (fn [period suffix]
                     (if-not (empty? period) (str period suffix) period))]
    (reduce str (reverse (map add-suffix (map period-to-word-cached partitioned-number) periods)))))

(defn range-english
  "Returns string representation of integer values in range
   from start to finish exclusive." 
  ([fin] 
   (range-english 0 fin))
  ([start fin] 
   (map num-to-word (range start fin)))) 

(defn solve [n]
  (reduce + (map count (range-english 1 (inc n)))))
