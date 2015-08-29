(ns project-euler.p171
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.math.combinatorics :refer [permutations]]
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
  (first (drop-while #(> max-digits (len %)) (iterate pad summand))))

(defn collector [max-digits] 
  (let [h (fn helper [v d sq]
            (println "loop" v d sq)
            (let [remaining (- v sq)]
              (cond (contains? @cache [v sq]) (get @cache [v sq]) 
                    (neg? remaining) [] 
                    (neg? d) []
                    (zero? remaining)(do (println "doing summand")[(->Summands [(sqrt v)])]) 
                    :else (let [cached (filter #(contains? @cache [remaining %]) (take-while (partial >= remaining) (map square (range 0 10))))
                                uncached (take-while (partial >= remaining) (remove (set cached) (map square (range 0 10))))
                                _ (println "uncached" uncached)
                                cached-base (mapcat #(get @cache [remaining %]) cached) 
                                _ (println "cached-base" cached-base)
                                base (mapcat (partial helper remaining (dec d)) (reverse uncached))
                                _ (println "base" base)
                                base2 (concat cached-base base)
                                _ (println "base2" base2)
                                _ (println "filtered" (filter #(>= max-digits (len %)) (map #(add % sq) (map reprotect base2))))
                                baz (filter #(>= max-digits (len %)) (map #(add % sq) (map reprotect base2))) 
                                _ (println "baz" baz)
                                ]
                            (when (not= 0 sq) (swap! cache assoc [v sq] baz))
                            baz
                            #_(get (swap! cache assoc [v sq] (filter #(>= max-digits (len %)) (map #(add % sq) (map reprotect base2))))
                                 [v sq])))))
        
        ]
    h))

(comment (do (def squares (possible-squares 1e20))
             (def digits (map square (range 10)))
             (def cache (atom {}))
             (def helpfn (collector (max-digits 1e20)))
             (def padfn (partial zero-padded-variants (max-digits 1e20))))
         (map #(helpfn % 25) (range 1 55))
         (time (count (reduce +' (map unwrap (flatten (map padfn (flatten (map #(map (partial helpfn %) digits) squares))))))))
         (time (count (reduce +' (map (comp unwrap padfn) (flatten (map #(map (partial helpfn % 18) digits) (range 4 5) #_(take 7 squares)))))))
         (time (count (reduce +' (sort (map unwrap (flatten (flatten (def foo (map #(map (partial helpfn % 18) digits) (range 4 5) #_(take 7 squares))))))))))
         (time (count (reduce +' (sort (count (map unwrap (flatten (map #(map (partial helpfn %) digits) [16] #_(take 3 squares)))))))))
         (time (count (map zero-padded-variants (take 250 (map unwrap (flatten (map #(map (partial helpfn %) digits) squares)))))))
         )

(comment (defn digits-range [max-digits n]
           [(max (int (/ n 81)) 1) (min n max-digits)]))

(comment (defn get-valid-digits [n [min-digits _]]
           (filter #(>= n (* min-digits %)) (range 1 10))))

