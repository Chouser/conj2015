(ns conj2015.common)

(defn find-all-logs []
  (map (fn [linesyms]
         (->> (mapcat list linesyms (repeat "\n"))
              (apply str)
              java.io.StringReader.))
       '[[a b c] [D bad1 F] [bad2 h i]]))

(defn analyze-entry [entry]
  (prn entry))

(defn well-formed-log-entry? [text]
  (not (.startsWith text "bad")))

;; (pst 3)
