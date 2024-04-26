(ns step1-read-print)

(defn repl-read [x] x)

(defn repl-eval [x] x)

(defn repl-print [x] x)

(defn repl-rep [x] x)

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