(ns project-euler.p025
  (:require [project-euler.p002 :as p002]))

(defn solve [] (first (drop-while #(> 1000 (count (str %) )) p002/fibonacci)))
