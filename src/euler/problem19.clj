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

(def start-day {:year 1900, :month 'January, :day-of-month 1})


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

(defn increment-day
  "Gets the next valid calendar date after this date."
  [{:keys [year month day-of-month], :as date}]
  (let [year-of-next-day (if (is-last-day-of-year? date) (inc year) year)
        month-of-next-day
          (if (is-last-day-of-month? date) (next-month month) month)
        day-of-month-of-next-day
          (if (is-last-day-of-month? date) 1 (inc day-of-month))])
  ;;tbd
)

(comment)

(defn day-of-week-of-day
  (loop [current-day start-day
         day-of-week 'Monday
         sunday-count 0]
    (let [next-day (increment-day current-day)]
      (recur next-day
             next-day-of-week
             (..)
             (if now-it-is-sunday (inc sunday-count) sunday-count)))))
