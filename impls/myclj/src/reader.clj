(ns reader
  (:require [clojure.string :as string]))

(defn rd-peek
  "Peek the first token in reader"
  [reader]
  (-> reader :tokens first))

(defn rd-advance
  "Just advance token."
  [reader]
  (update reader :tokens next))

(defn rd-next
  "Get and advance token in reader.
   Returns [token, updated reader]"
  [reader]
  [(rd-peek reader) (rd-advance reader)])

(declare read-form)

(defn read-list [reader]
  (let [r' (rd-advance reader) ;; Consume '('
        go (fn [reader rforms]
             (case (rd-peek reader)
               ")" (let [reader' (rd-advance reader)] ;; Consume ')'
                     [(reverse rforms) reader'])
               (let [[form reader'] (read-form reader)
                     rforms' (cons form rforms)]
                 (recur reader' rforms'))))]
    (go r' nil)))

(defn read-atom
  "If a token is not a number, it's a symbol."
  [reader]
  (let [[token reader'] (rd-next reader)
        atom (if (re-find #"[^0-9]" token)
               token
               (Integer/parseInt token))]
    [atom reader']))

(defn read-form [reader]
  (case (rd-peek reader)
    "(" (read-list reader)
    (read-atom reader)))

(defn tokenize
  "Tokenizes a string into a sequence"
  [s]
  (let [m (re-seq
           #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"
           s)]
    (->>
     m
     (map first)
     (map string/trim)
     (filter not-empty))))

(defn read-str
  "Creates a reader from string"
  [s]
  (let [reader {:tokens (tokenize s)}
        [form _] (read-form reader)]
    form))

(comment
  (read-str "123")
  (read-str "   123 ")
  (read-str "abc")
  (read-str " abc ")
  (read-str "()")
  (read-str "(123 456)")
  (read-str "(  123  456 789)")
  (read-str "(+ 1 2)")
  (read-str "(   + 2    (*  3  4))")
  (read-str "(123 abc)")
  (read-str ",1")
  :rcf)