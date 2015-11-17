(ns conj2015.n02-restarts
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [slingshot.core :refer [throw+]]
            [swell.api :refer [restart-case handler-bind invoke-restart]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (throw+ {:type ::malformed-log-entry, :text text}
            "Log entry was malformed; could not parse.")))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(restart-case
            [:skip-log-entry (fn [] nil)]
            (parse-log-entry %))
          lines)))

(defn analyze-log [log]
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (handler-bind
   [#(= ::malformed-log-entry (:type %))
    (fn [e]
      (invoke-restart :skip-log-entry))]
   (doseq [log (find-all-logs)]
     (analyze-log log))))

;; conj2015.n02-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil
