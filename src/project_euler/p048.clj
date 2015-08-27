(ns project-euler.p048
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]))

;;; The series, 11 + 22 + 33 + ... + 1010 = 10405071317.
;;; Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

(defn solve []
  (s/reverse (subs (s/reverse (str (r/fold + (r/map #(expt % %) (range 1 1000))))) 0 10)))


