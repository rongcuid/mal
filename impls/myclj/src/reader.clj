(ns reader
  (:require [clojure.string :as string]))

(defn rd-peek
  "Peek and return the first token in reader"
  [reader]
  (-> reader :tokens first))

(defn rd-advance
  "Advance and return updated reader."
  [reader]
  (update reader :tokens next))

(defn rd-next
  "Get token and advance reader."
  [reader]
  {:token (rd-peek reader)
   :reader (rd-advance reader)})

; Forward declare the top-level form
(declare read-form)

(defn- read-return
  "Advance reader and return result"
  [reader result]
  {:form result :reader (rd-advance reader)})

(defn- read-nil [reader]
  (read-return reader nil))

(defn- read-true [reader]
  (read-return reader true))

(defn- read-false [reader]
  (read-return reader false))

(defn- read-form-until [reader until]
  (let
   [go (fn [reader rforms]
         (let
          [tk (rd-peek reader)]
           (cond
             (nil? tk) (throw (IllegalArgumentException. "EOF"))
             (= until tk) {:form (reverse rforms) :reader reader}
             :else (let
                    [{form :form reader' :reader} (read-form reader)
                     rforms' (cons form rforms)]
                     (recur reader' rforms')))))]
    (go reader nil)))

(defn- read-bracketed-form [reader l r]
  (let
   [r' (rd-advance reader) ;; Consume left
    {form :form r'' :reader} (read-form-until r' r)
    r''' (rd-advance r'')]
    {:form form :reader r'''}))

(defn- read-list [reader]
  (read-bracketed-form reader "(" ")"))

(defn- read-vector [reader]
  (read-bracketed-form reader "[" "]"))

(defn- read-atom
  "If a token is not a number, it's a symbol."
  [reader]
  (let [{token :token reader' :reader} (rd-next reader)
        atom (if (re-find #"[^0-9]" token)
               token
               (Integer/parseInt token))]
    {:form atom :reader reader'}))

(defn read-form
  "Top level form parsing."
  [reader]
  (case (rd-peek reader)
    "nil" (read-nil reader)
    "true" (read-true reader)
    "false" (read-false reader)
    "(" (read-list reader)
    "[" (read-vector reader)
    (read-atom reader)))

(defn tokenize
  "Tokenizes a string into a sequence"
  [s]
  (let [re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)"
        go (fn [s rtokens]
             (let [[consumed token] (re-find re s)]
               (if (empty? token)
                 (reverse rtokens)
                 (recur
                  (subs s (count consumed))
                  (cons token rtokens)))))]
    (go s nil)))

(defn read-str
  "Creates a reader from string"
  [s]
  (try
    (let [reader {:tokens (tokenize s)}
          {form :form} (read-form reader)]
      form)
    (catch IllegalArgumentException e
      (println (.getMessage e)))))

(comment
  (read-str "nil")
  (read-str "true")
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
  (read-str ",1,,,")
  (read-str "(")
  :rcf)