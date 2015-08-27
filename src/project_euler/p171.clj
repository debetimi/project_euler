(ns project-euler.p171)

;;; For a positive integer n, let f(n) be the sum of the squares of the digits  (in base 10) of n, e.g.
;;; f(3) = 32 = 9,
;;; f(25) = 22 + 52 = 4 + 25 = 29,
;;; f(442) = 42 + 42 + 22 = 16 + 16 + 4 = 36
;;; Find the last nine digits of the sum of all n, 0 < n < 1020, such that f(n) is a perfect square.

(defn square [x] (* x x))

(defn max-digits [limit]
  (dec (int (Math/log10 limit))))

(defprotocol IProtect
  (unwrap [this] "unwraps value"))

(defrecord Summands [summands]
  IProtect
  (unwrap [this] (:summands this)))

(defn possible-squares [limit]
  (take-while (partial >= (* 9 9 (max-digits limit))) (map (comp square inc) (range))))

(def cache (atom {}))

(defn get-help-fn [max-digits squares] 
  (let [helper (fn helper [v sq coll]
                 (let [remaining (- v sq)]
                   (cond (or (neg? remaining) (< max-digits (count coll))) [] 
                         (zero? remaining) (->Summands (conj coll sq))
                         :else (flatten (map #(helper remaining % (conj coll sq)) (take-while (partial >= sq) squares))))))
        wrapper (fn [v sq]
                  (when-not (contains? @cache [v sq])
                    (swap! cache assoc [v sq] (helper v sq [])))
                  (get @cache [v sq]))
        ]
    wrapper))

(comment (defn digits-range [max-digits n]
           [(max (int (/ n 81)) 1) (min n max-digits)]))

(comment (defn get-valid-digits [n [min-digits _]]
           (filter #(>= n (* min-digits %)) (range 1 10))))

