(ns conj2015.n01-handler-case
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (throw (ex-info (str "Log entry was malformed;"
                         " could not parse.")
                    {:type ::malformed-log-entry
                     :text text}))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(try
             (parse-log-entry %)
             (catch clojure.lang.ExceptionInfo e
               (if (= ::malformed-log-entry
                      (:type (ex-data e)))
                 nil ;; Skip bad entries
                 (throw e))))
          lines)))

(defn analyze-log [log]
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (doseq [log (find-all-logs)]
    (analyze-log log)))

;; conj2015.n01-handler-case> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil
