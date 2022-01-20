(ns euler.problem19)

;; You are given the following information, but you may prefer to do some
;; research for yourself.

;;     1 Jan 1900 was a Monday.
;;     Thirty days has September,
;;     April, June and November.
;;     All the rest have thirty-one,
;;     Saving February alone,
;;     Which has twenty-eight, rain or shine.
;;     And on leap years, twenty-nine.
;;     A leap year occurs on any year evenly divisible by 4, but not on a
;;     century unless it is divisible by 400.

;; How many Sundays fell on the first of the month during the twentieth century
;; (1 Jan 1901 to 31 Dec 2000)?

(def thirty-day-months #{'September 'April 'June 'November})

(def months-of-year
  ['January 'February 'March 'April 'May 'June 'July 'August 'September 'October
   'November 'December])

(def days-of-week
  ['Sunday 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday])
(clojure.core/cycle days-of-week)
(defn is-leap-year? [year] (zero? (mod year 4)))

(comment (is-leap-year? 1900)
         (is-leap-year? 1901)
         (is-leap-year? 1902)
         (is-leap-year? 1903)
         (is-leap-year? 1904)
         (is-leap-year? 2000))


(defn days-in-month
  [year month]
  (cond (thirty-day-months month) 30
        (= 'February month) (if (is-leap-year? year) 29 28)
        :else 31))

(comment (let [year 1900]
           (for [month months-of-year] [month (days-in-month year month)])))

(def beginning-of-time
  {:year 1900, :month 'January, :day-of-month 1, :day-of-week 'Monday})
(def start-date
  (last (days-between beginning-of-time
                      {:year 1901, :month 'January, :day-of-month 2})))
(def end-date {:year 2001, :month 'January, :day-of-month 1})


(def test-dates
  [{:year 1902, :month 'February, :day-of-month 28}
   {:year 2022, :month 'January, :day-of-month 20}
   {:year 2022, :month 'December, :day-of-month 31}
   {:year 2022, :month 'January, :day-of-month 31}])

(defn is-last-day-of-month?
  [{:keys [month day-of-month year]}]
  (= day-of-month (days-in-month year month)))

(comment (for [test-date test-dates]
           [test-date (is-last-day-of-month? test-date)]))

(defn is-last-day-of-year?
  [{:keys [month day-of-month], :as date}]
  (and (= month 'December) (is-last-day-of-month? date)))

(comment (for [test-date test-dates]
           [test-date (is-last-day-of-year? test-date)]))

(defn next-month
  [month]
  (->> (clojure.core/cycle months-of-year)
       (take 13)
       (drop-while (partial not= month))
       (drop 1)
       first))

#_(next-month 'December)

(defn next-day-of-week
  [day-of-week]
  (->> (clojure.core/cycle days-of-week)
       (take 8)
       (drop-while (partial not= day-of-week))
       (drop 1)
       first))

#_(next-day-of-week 'Sunday)
#_(next-day-of-week 'Saturday)

(defn increment-day
  "Gets the next valid calendar date after this date."
  [{:keys [year month day-of-month day-of-week], :as date}]
  {:year (if (is-last-day-of-year? date) (inc year) year),
   :month (if (is-last-day-of-month? date) (next-month month) month),
   :day-of-month (if (is-last-day-of-month? date) 1 (inc day-of-month)),
   :day-of-week (next-day-of-week day-of-week)})

(comment (for [test-date test-dates]
           [test-date (increment-day (assoc test-date :day-of-week 'Monday))]))

(defn dates-are-equal?
  "A weakness of our model is the need to specify the day of week of the end date.
  This shouldn't sink our program, so we'll make things work even if
  the end date day of week is incorrect."
  [a-date b-date]
  (= (dissoc a-date :day-of-week) (dissoc b-date :day-of-week)))


(defn days-between
  [start-date end-date]
  (->> start-date
       (iterate increment-day)
       (take-while (complement (partial dates-are-equal? end-date)))))


(->> (days-between start-date end-date)
     (filter (fn [{:keys [day-of-week day-of-month]}]
               (and (= day-of-week 'Sunday) (= day-of-month 1))))
     count)



