(ns project-euler.p020)

(defn solve [] (-> (reduce *' (range 1 101)) str (clojure.string/split #"") rest ((partial map read-string)) ((partial reduce +')))) 

