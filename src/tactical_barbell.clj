(ns tactical-barbell
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [cheshire.core :refer :all])
  (:import [java.lang Math]
           [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(defn brzycki-formula [weight reps] (int (* weight (/ 36 (- 37 reps)))))


(def lifts '(:bench :incline-bench :front-squat :back-squat :chin-ups :pull-ups :deadlift :press))

(defn read-file [] (parse-stream (io/reader (io/resource "max.json")) keyword))
(defn latest-max-day [data] (map #(hash-map (first %)
                                            (dissoc (first (sort-by
                                                             (fn [x] (LocalDate/parse (get x "date") (DateTimeFormatter/ofPattern "M/d/y")))
                                                             (comp - compare) (second %))) "date")) data))

(defn person-lift-maxes [max-day]
  (into {} (map #(hash-map (first %)
                           (into {} (map (fn [[k v]] {k (brzycki-formula (:weight v) (:reps v))})
                                         (select-keys (second %) lifts)))) (apply merge max-day))))

(def templates {:operator {70 {:sets 5 :reps 5}
                           80 {:sets 5 :reps 5}
                           90 {:sets 4 :reps 3}
                           75 {:sets 5 :reps 5}
                           85 {:sets 5 :reps 3}
                           95 {:sets 4 :reps 2}}})

(def difficulty {:im-too-young-to-die #(Math/floor %)
                 :hurt-me-plenty      #(Math/ceil %)})

(defn calculate-weight [level percent max]
  (int (* ((level difficulty) (Math/abs (float (/ (* max (/ percent 100)) 5)))) 5)))

(defn weeks [maxes level template]
  (into (sorted-map) (apply merge (map (fn [[lifter lifts]]
                                         (hash-map lifter (map (fn [[percent routine]]
                                                                 (merge {} {:percent percent :sets-reps (str (:sets routine) "x" (:reps routine))}
                                                                        (into {} (map (fn [[lift weight]] [lift (calculate-weight level percent weight)]) lifts))))
                                                               (template templates)))) maxes))))

(defn -main
  "Generates Operator Block From Lastest Max Date"
  []
  (let [w (weeks (person-lift-maxes (latest-max-day (read-file))) :im-too-young-to-die :operator)]
    (doseq [[lifter block] w]
      (println lifter)
      (pprint/print-table block)
      (println))))

