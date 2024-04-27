(ns printer
  (:require [clojure.string :as string]))

(declare pr-str)

(defn- pr-body [mal]
  (string/join " " (map pr-str mal)))

(defn- pr-bracket [l body r]
  (string/join "" (list l body r)))

(defn pr-str
  "String representation of Mal data structure"
  [mal]
  (cond
    (nil? mal) "nil"
    (seq? mal) (pr-bracket "(" (pr-body mal) ")")
    (vector? mal) (pr-bracket "[" (pr-body mal) "]")
    :else (str mal)))