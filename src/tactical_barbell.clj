(ns tactical-barbell
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.math :refer [ceil]])
  (:import
    (java.lang
      Math)
    (java.time
      LocalDate)
    (java.time.format
      DateTimeFormatter)))

(def max-days (edn/read-string (slurp (io/resource "max.edn"))))

(def templates
  {:operator
   [{:percent 70 :sets 5 :reps 5}
    {:percent 80 :sets 5 :reps 5}
    {:percent 90 :sets 4 :reps 3}
    {:percent 75 :sets 5 :reps 5}
    {:percent 85 :sets 5 :reps 3}
    {:percent 95 :sets 4 :reps 2}]
   :fighter
   [{:percent 75 :sets 5 :reps 5}
    {:percent 80 :sets 5 :reps 5}
    {:percent 90 :sets 5 :reps 3}
    {:percent 75 :sets 5 :reps 5}
    {:percent 80 :sets 5 :reps 5}
    {:percent 90 :sets 5 :reps 3}]})


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
  (* (ceil (abs (float (/ (* max (/ percent 100)) 2.5)))) 2.5))


(defn weeks
  [maxes template]
  (into (sorted-map) (apply merge (map (fn [[lifter lifts]]
                                         (hash-map lifter (map (fn [routine]
                                                                 (merge {} {:percent (:percent routine) :sets-reps (str (:sets routine) "x" (:reps routine))}
                                                                        (into {} (map (fn [[lift weight]] [lift (calculate-weight (:percent routine) weight)]) lifts))))
                                                               (template templates)))) maxes))))


(defn -main
  "Generates Block From Lastest Max Date"
  []
  (let [w (weeks (person-lift-maxes (latest-max-day max-days)) :fighter)]
    (doseq [[lifter block] w]
      (println lifter)
      (pprint/print-table block)
      (println))))
