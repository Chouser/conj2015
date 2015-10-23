(ns conj2015.n03-kit-multi-restarts
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [net.n01se.error-kit :refer [deferror with-handler raise
                                         bind-continue handle continue]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(deferror MalformedLogEntry [] [msg text])

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (with-handler
      (raise MalformedLogEntry "Log entry was malformed; could not parse." text)
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
              nil))
          lines)))

(defn analyze-log [log]
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (with-handler
     (doseq [log (find-all-logs)]
       (analyze-log log))
     (handle MalformedLogEntry [text msg]
       (continue no-such-restart))))

;;        (continue use-value {:failed-to-parse text}))))
;; conj2015.n03-kit-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:failed-to-parse "bad1"}
;; {:successfully-parsed "F"}
;; {:failed-to-parse "bad2"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil

;;        (continue reparse-entry (str "better than " text)))))
;; conj2015.n03-kit-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "better than bad1"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "better than bad2"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil

;;        (continue skip-log-entry))))
;; conj2015.n03-kit-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil

;;        (continue no-such-restart))))
;; conj2015.n03-kit-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {skip-log-entry {:rfunc #object[conj2015.n03_kit_multi_restarts$parse_log_file$fn__1921$fn__1923 0x4b83b34e "conj2015.n03_kit_multi_restarts$parse_log_file$fn__1921$fn__1923@4b83b34e"], :blockid G__1922}, use-value {:rfunc #object[conj2015.n03_kit_multi_restarts$parse_log_entry$fn__1911 0x3abe51cd "conj2015.n03_kit_multi_restarts$parse_log_entry$fn__1911@3abe51cd"], :blockid G__1910}, reparse-entry {:rfunc #object[conj2015.n03_kit_multi_restarts$parse_log_entry$fn__1913 0x24cb8f74 "conj2015.n03_kit_multi_restarts$parse_log_entry$fn__1913@24cb8f74"], :blockid G__1910}}
;; Exception Unbound continue name no-such-restart  net.n01se.error-kit/raise* (error_kit.clj:107)
