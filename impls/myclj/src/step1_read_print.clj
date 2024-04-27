(ns step1-read-print
  (:require [reader :as r]
            [printer :as p]))

(defn repl-read [s]
  (r/read-str s))

(defn repl-eval [x] x)

(defn repl-print [mal]
  (p/pr-str mal))

(defn repl-rep [x]
  (-> x repl-read repl-eval repl-print))

(defn main
  []
  (print "user> ")
  (flush)
  (let [line (read-line)]
    (if (nil? line)
      nil
      (let [res (repl-rep line)]
        (println res)
        (recur)))))

(defn -main [& args] (main))