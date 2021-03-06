(ns project-euler.p031)

;;; In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
;;; 1p, 2p, 5p, 10p, 20p, 50p, £1  (100p) and £2  (200p).
;;; It is possible to make £2 in the following way:
;;; 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
;;; How many different ways can £2 be made using any number of coins?

(def currencies {"GBP" [1 2 5 10 20 50 100 200]
                 "USD" [1 5 10 25 50 100]})

(defn num-ways-to-make-change
  [amt & {:keys [iso], :or {iso "USD"}}]
  (let [ways (atom (vec (repeat amt 0)))]
    (doseq [coin (get currencies iso)]
      (doseq [n (range (dec coin) amt)]
        (let [curr (get @ways n)
              increment (get @ways (- n coin) 1)]
          (swap! ways assoc n (+' curr increment)))))
    (get @ways (dec amt))))

(defn solve [] (num-ways-to-make-change 200 :iso "GBP"))
