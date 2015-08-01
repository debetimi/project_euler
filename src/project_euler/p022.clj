(ns project-euler.p022
  (:require [clojure.string :as s]))

;;; https://projecteuler.net/problem=22
;;; Using names.txt  (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
;;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
;;; What is the total of all the name scores in the file?

;;; Map of character to value e.g. A -> 1
;;; A-Z are char values 65-90
(def char->val (zipmap (map char (range 65 91)) (map inc (range))))

;; Could have modified the file to add [] around the string and just done read-string
;; but wanted to solve without modifying resources
(def sorted-names (sort (map read-string (s/split (slurp "resources/p022_names.txt") #","))))

(defn word->value 
  [word] 
  (reduce + (map char->val (seq word))))

(defn solve 
  [] 
  (reduce +' (map * (map word->value sorted-names) (map inc (range)))))
