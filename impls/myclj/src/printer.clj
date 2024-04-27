(ns printer
  (:require [clojure.string :as string]))

(defn pr-str
  "String representation of Mal data structure"
  [mal]
  (if (seq? mal)
    (let [body (string/join " " (map pr-str mal))]
      (string/join "" (list "(" body ")")))
    (str mal)))