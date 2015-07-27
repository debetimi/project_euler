(ns project-euler.p017
  (:require [clojure.string :as s]))

(def singles {:0 "" :1 "one" :2 "two" :3 "three" :4 "four" :5 "five" :6 "six" :7 "seven" :8 "eight" :9 "nine"})
(def teens {:0 "ten" :1 "eleven" :2 "twelve" :3 "thirteen" :4 "fourteen" :5 "fifteen" :6 "sixteen" :7 "seventeen" :8 "eighteen" :9 "nineteen"})
(def tens {:0 "" :1 "" :2 "twenty" :3 "thirty" :4 "forty" :5 "fifty" :6 "sixty" :7 "seventy" :8 "eighty" :9 "ninety"})

(def outer-suffixes ["" "thousand" "million" "billion"])
(def inner-suffixes ["hundred" "" ""])
(def order (cycle [singles tens]))

(defn range-english
  "returns string representation of integer values in range
   from start to finish exclusive." 
  ([fin] 
   (range-english 0 fin))
  ([start fin] 
   (let [partition-nums (map (partial partition 3 3 []) (map (comp s/reverse str) (range start fin)))]
     (loop [numbers partition-nums coll []]
       (if (empty? numbers)
         coll
         (let [number (first numbers)
               num-rep (loop [periods number str-rep "" suffixes outer-suffixes]
                         (if (empty? periods)
                           str-rep 
                           (let [period-rep (let [period (first periods)
                                                  size (count period)
                                                  words (take size (drop (dec size) order))
                                                  suffixes (drop (- 3 size) inner-suffixes)] 
                                              (loop [digits (reverse period) words words period-rep "" suffixes suffixes prefix ""]
                                                (if (empty? digits) 
                                                  period-rep
                                                  (let [kw (keyword (str (first digits)))
                                                        word (kw (first words))
                                                        suffix (if-not (empty? word) (str (first suffixes)) "")]
                                                    (recur (rest digits) 
                                                           (if-not (and (not= :0 kw) (empty? word))
                                                             (rest words) 
                                                             (list teens)) 
                                                           (str period-rep (if-not (empty? word) prefix "") word suffix)
                                                           (rest suffixes)
                                                           (if-not (empty? suffix) "and" (if-not  (empty? word) "" prefix)))))))]
                             (recur (rest periods) (str period-rep (first suffixes) str-rep) (rest suffixes)))))]
           (recur (rest numbers) (conj coll num-rep)))))))) 

(defn- solve []
  (reduce + (map count (range-english 1 1001))))
