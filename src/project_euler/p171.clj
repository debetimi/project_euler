(ns project-euler.p171
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

;;; For a positive integer n, let f(n) be the sum of the squares of the digits  (in base 10) of n, e.g.
;;; f(3) = 32 = 9,
;;; f(25) = 22 + 52 = 4 + 25 = 29,
;;; f(442) = 42 + 42 + 22 = 16 + 16 + 4 = 36
;;; Find the last nine digits of the sum of all n, 0 < n < 1020, such that f(n) is a perfect square.

(defn square [x] (* x x))

(defn max-digits [limit]
  (dec (int (Math/log10 limit))))

(defprotocol IProtect
  (unwrap [this] "unwraps value")
  (len [this] "the size of this")
  (add [this x] "adds an element"))

(defrecord Summands [summands]
  IProtect
  (unwrap [this] (:summands this))
  (len [this] (count (:summands this)))
  (add [this summand] (update this :summands conj (sqrt summand))))

(defn possible-squares [limit]
  (take-while (partial >= (* 9 9 (max-digits limit))) (map (comp square inc) (range))))

(def cache (atom {}))

(defn collector [max-digits] 
  (let [helper(fn helper [v sq]
                (let [remaining (- v sq)]
                  (cond (contains? @cache [v sq])  (get @cache [v sq])
                        (neg? remaining) [] 
                        (zero? remaining) (->Summands [(sqrt v)])
                        :else (let [base (if (contains? @cache [remaining sq])
                                           (get @cache [remaining sq]) 
                                           (flatten (map (partial helper remaining)
                                                         (take-while (and (partial >= sq)
                                                                          (partial >= remaining))
                                                                     (map square (range 1 10))))))]
                                (get (swap! cache assoc [v sq] (map #(add % sq) (filter #(> max-digits (len %)) base)))
                                     [v sq])))))]
    helper))


(comment (def squares (possible-squares 10e20))
         (def cache (atom {}))
         (def helpfn (collector 20))
         (map #(helpfn % 25) (range 1 55))
         (time (count (map unwrap (flatten (map #(map (partial helpfn %) digits) squares)))))
         )

(comment (defn digits-range [max-digits n]
           [(max (int (/ n 81)) 1) (min n max-digits)]))

(comment (defn get-valid-digits [n [min-digits _]]
           (filter #(>= n (* min-digits %)) (range 1 10))))

