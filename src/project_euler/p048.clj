(ns project-euler.p048
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [clojure.math.numeric-tower :refer [expt]]))

;;; The series, 11 + 22 + 33 + ... + 1010 = 10405071317.
;;; Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

(defn solve []
  (mod (bigint (r/fold + (r/map #(expt ^Integer %  ^Integer %) (range 1 1000)))) 10000000000N))
