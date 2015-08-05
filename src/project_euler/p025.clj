(ns project-euler.p025
  (:require [project-euler.p002 :as p002]))

;;; What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

(defn solve [] (count (take-while #(> 1000 (count (str %) )) p002/fibonacci)))
