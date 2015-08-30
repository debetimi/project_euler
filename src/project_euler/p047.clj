(ns project-euler.p047
  (:require [project-euler.utils :refer [prime-factors]]))

(defn solve []
  (first (filter #(every? (partial <= 4) (map (comp count set prime-factors) %)) (partition 4 1 (range)))))
