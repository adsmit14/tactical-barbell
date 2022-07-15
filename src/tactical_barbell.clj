(ns tactical-barbell
  (:require
    [cheshire.core :refer [parse-stream]]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint])
  (:import
    (java.lang
      Math)
    (java.time
      DayOfWeek
      LocalDate)
    (java.time.format
      DateTimeFormatter)
    (net.fortuna.ical4j.data
      CalendarOutputter)
    (net.fortuna.ical4j.model
      Calendar)))


(def templates
  {:operator {70 {:sets 5 :reps 5}
              80 {:sets 5 :reps 5}
              90 {:sets 4 :reps 3}
              75 {:sets 5 :reps 5}
              85 {:sets 5 :reps 3}
              95 {:sets 4 :reps 2}}})


(def lifts
  '(:bench
     :incline-bench
     :front-squat
     :back-squat
     :chin-ups
     :pull-ups
     :deadlift
     :press))


(defn brzycki-formula
  [weight reps]
  (int (* weight (/ 36 (- 37 reps)))))


(defn read-file
  []
  (parse-stream (io/reader (io/resource "max.json")) keyword))


(defn sort-by-date-descending
  [days]
  (sort-by
    (fn [x] (LocalDate/parse (:date x) (DateTimeFormatter/ofPattern "M/d/y")))
    (comp - compare) days))


(defn latest-max-day
  [data]
  (map #(let [person (first %) days (second %)]
          (hash-map person (first (sort-by-date-descending days)))) data))


(defn person-lift-maxes
  [max-day]
  (into {} (map #(hash-map (first %)
                           (into {} (map (fn [[k v]] {k (brzycki-formula (:weight v) (:reps v))})
                                         (select-keys (second %) lifts)))) (apply merge max-day))))


(defn calculate-weight
  [percent max]
  (* (Math/floor (Math/abs (float (/ (* max (/ percent 100)) 2.5)))) 2.5))


(defn weeks
  [maxes template]
  (into (sorted-map) (apply merge (map (fn [[lifter lifts]]
                                         (hash-map lifter (map (fn [[percent routine]]
                                                                 (merge {} {:percent percent :sets-reps (str (:sets routine) "x" (:reps routine))}
                                                                        (into {} (map (fn [[lift weight]] [lift (calculate-weight percent weight)]) lifts))))
                                                               (template templates)))) maxes))))


(defn -main
  "Generates Operator Block From Lastest Max Date"
  []
  (let [dayOfWeek (DayOfWeek/MONDAY) w (weeks (person-lift-maxes (latest-max-day (read-file))) :operator)]
    (doseq [[lifter block] w]
      (println lifter)
      (pprint/print-table block)
      (println))))


;; function that takes current date, week index, and using the dayofweek finds the
;; first date that falls on and creates an event for that date plus one 48 and 96hrs after.

;; iterate over block and create 3 events for each week -- in those events plop the details
;; then create the calendar and add the events

(comment (LocalDate/now))
(comment (-main))
