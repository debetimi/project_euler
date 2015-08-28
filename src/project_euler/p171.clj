(ns project-euler.p171
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [project-euler.utils :refer [digits->num]]))

;;; For a positive integer n, let f(n) be the sum of the squares of the digits  (in base 10) of n, e.g.
;;; f(3) = 32 = 9,
;;; f(25) = 22 + 52 = 4 + 25 = 29,
;;; f(442) = 42 + 42 + 22 = 16 + 16 + 4 = 36
;;; Find the last nine digits of the sum of all n, 0 < n < 1020, such that f(n) is a perfect square.

(defn square [x] (* x x))

(defn max-digits [limit]
    (dec (int (Math/log10 limit))))

;; Wraps array to protect it from being flattened 
(defprotocol IProtect
  (unwrap [this] "unwraps value")
  (len [this] "the size of this")
  (add [this x] "adds an element")
  (pad [this] "zero pads element"))

(defrecord Summands [summands]
  IProtect
  (unwrap [this] (digits->num (:summands this)))
  (len [this] (count (:summands this)))
  (add [this summand] (assoc this :summands (vec (concat [(sqrt summand)] (:summands this)))))
  (pad [this] (update this :summands conj 0)))

(defn possible-squares [limit]
  (take-while (partial >= (* 9 9 (max-digits limit))) (map (comp square inc) (range))))

;; for some reason there is a bug
;; in clojure that unwraps some of these records into a map entry.
;; so my summons suddenly becomes a map entry
(defn reprotect [s]
  (if-not (= Summands (type s)) (->Summands (:summands s)) s))

(def cache (atom {}))

(defn zero-padded-variants [max-digits ^Summands summand]
  (take-while #(>= max-digits (len %)) (iterate pad summand)))

(defn collector [max-digits] 
  (let [h (fn helper [v sq]
            (let [remaining (- v sq)]
              (when (= sq 36) (println "v" v))
              (cond (contains? @cache [v sq]) (get @cache [v sq]) 
                    (neg? remaining) [] 
                    (zero? remaining) [(get (swap! cache assoc [v sq] (->Summands [(sqrt v)])) [v sq])]
                    :else (let [base (if(contains? @cache [remaining sq]) 
                                       (do (when (= [40 9] [remaining sq]) (println "HIT2" remaining sq)) (get @cache [remaining sq])) 
                                       (flatten (map (partial helper remaining)
                                                     (take-while (and #_(partial >= sq)
                                                                      (partial >= remaining))
                                                                 (map square (range 1 10))))))]
                            (when (= [9] [sq]) (println v sq remaining base))
                            #_(when (= [40 9] [v sq]) (println "O1" base))
                            (get (swap! cache assoc [v sq] (map #(add % sq) (filter #(> max-digits (len %)) (map reprotect base))))
                                 [v sq])))))]
    h))


(comment (def squares (possible-squares 1e20))
         (def digits (map square (range 1 10)))
         (def cache (atom {}))
         (def helpfn (collector (max-digits 1e20)))
         (def padfn (partial zero-padded-variants (max-digits 1e20)))
         (map #(helpfn % 25) (range 1 55))
         (time (count (reduce +' (map unwrap (flatten (map padfn (flatten (map #(map (partial helpfn %) digits) squares))))))))
         (time (count (reduce +' (sort (map unwrap (flatten (map padfn (flatten (map #(map (partial helpfn %) digits) [49] #_(take 7 squares))))))))))
         (time (count (map zero-padded-variants (take 250 (map unwrap (flatten (map #(map (partial helpfn %) digits) squares)))))))
         )

(comment (defn digits-range [max-digits n]
           [(max (int (/ n 81)) 1) (min n max-digits)]))

(comment (defn get-valid-digits [n [min-digits _]]
           (filter #(>= n (* min-digits %)) (range 1 10))))

