(ns matr-core.utils
  (:require [datascript.core :as d]))

(defn juxt-map
  "Given a map j from keys k to functions f and an object e, return new
  map populated with those keys k and values computed by calling each
  of the corresponding functions f in j on e.

  Useful for transforming trees of maps. Also has a single-argument
  curried form."
  ([j e] (into {} (map (fn [[k f]] [k (f e)]) j)))
  ([j] (fn [e] (juxt-map j e))))

(defn db-since
  "Filter a db to just the datoms after a particular transaction."
  [db tx]
  (d/filter db (fn [db datom] (< tx (:tx datom)))))
(defn db-restricted
  "Filter a db to just the datoms involving a given set of entities"
  [db entities]
  (d/filter db (fn [db datom] (contains? entities (:e datom)))))
