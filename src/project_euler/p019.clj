(ns project-euler.p019)

;;; You are given the following information, but you may prefer to do some research for yourself.
;;; 
;;; 1 Jan 1900 was a Monday.
;;; Thirty days has September,
;;; April, June and November.
;;; All the rest have thirty-one,
;;; Saving February alone,
;;; Which has twenty-eight, rain or shine.
;;; And on leap years, twenty-nine.
;;; A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
;;; How many Sundays fell on the first of the month during the twentieth century  (1 Jan 1901 to 31 Dec 2000)?

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

(defn days-from
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
        jan-1-1901 (first (drop-while #(> 1901 (:year %)) (days-from start)))]
    (count (filter sunday-and-first? (take-while twentieth-century? (days-from jan-1-1901))))))
