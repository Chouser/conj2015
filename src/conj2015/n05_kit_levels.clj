(ns conj2015.n05-kit-levels
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [net.n01se.error-kit :refer [deferror with-handler raise
                                         bind-continue handle continue]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(def ^:dynamic *level* :top)

(deferror MalformedLogEntry [] [msg text])

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (with-handler
      (binding [*level* :low]
        (raise MalformedLogEntry "Log entry was malformed; could not parse." text))
      (bind-continue use-value [value]
        value)
      (bind-continue reparse-entry [fixed-text]
        (parse-log-entry fixed-text)))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(with-handler
            (parse-log-entry %)
            (bind-continue skip-log-entry []
                           (raise MalformedLogEntry "msg" "again!")))
          lines)))

(defn analyze-log [log]
  (binding [*level* :mid]
    (doseq [entry (parse-log-file log)]
      (analyze-entry entry))))

(defn log-analyzer []
  (binding [*level* :high]
    (with-handler
      (doseq [log (find-all-logs)]
        (analyze-log log))
      (handle MalformedLogEntry [text msg]
              (continue skip-log-entry)))))

;;               (continue use-value *level*)))))
;; Exception Unbound continue name skip-log-entry  net.n01se.error-kit/raise* (error_kit.clj:107)

;;               (continue skip-log-entry)))))
;; conj2015.n05-kit-levels> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {}
;; Exception Unbound continue name skip-log-entry  net.n01se.error-kit/raise* (error_kit.clj:107)
