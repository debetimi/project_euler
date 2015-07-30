(ns project-euler.p019)

(def months [:Jan :Feb :Mar :Apr :May :June :July :Aug :Sept :Oct :Nov :Dec])
(def days-of-week [:Sun :Mon :Tues :Wed :Thur :Fri :Sat])

(defn leap-year? 
  [year]
  (if-not (zero? (mod year 100)) 
    (zero? (mod year 4))
    (zero? (mod year 400))))

(defn month->num-days
  [month year]
  (case month
    (:Apr :June :Sept :Nov) 30
    :Feb (if (leap-year? year) 29 28)
    31))

(defn days-since
  "lazy sequence of days starting at start"
  [start]
  (letfn [(get-date [date]
            (let [{:keys [day month year day-of-week]} date
                  last-day-of-month? (= day (month->num-days month year)) 
                  last-day-of-year? (and last-day-of-month? (= :Dec month))
                  next-day (if last-day-of-month? 1 (inc day)) 
                  next-month (if last-day-of-month? (second (drop-while (partial not= month) (cycle months))) month)
                  next-year (if last-day-of-year? (inc year) year)
                  next-day-of-week (second (drop-while (partial not= day-of-week) (cycle days-of-week)))]
              (cons date (lazy-seq (get-date {:day next-day,  :month next-month, :year next-year, :day-of-week next-day-of-week})))))]
    (get-date start)))

(defn solve
  []
  (let [sunday-and-first? (fn [date]
                            (and (= :Sun (:day-of-week date)) (= 1 (:day date))))
        start {:day 1, :month :Jan :year 1900 :day-of-week :Mon}
        twentieth-century? #(> 2001 (:year %))
        jan-1-1901 (first (drop-while #(> 1901 (:year %)) (days-since start)))]
    (count (filter sunday-and-first? (take-while twentieth-century? (days-since jan-1-1901))))))
