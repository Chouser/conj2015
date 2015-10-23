(ns conj2015.n03-multi-restarts
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]
            [slingshot.core :refer [throw+]]
            [swell.api :refer [restart-case handler-bind invoke-restart]]))

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (restart-case
     [:use-value (fn [value]
                   value)
      :reparse-entry (fn [fixed-text]
                       (parse-log-entry fixed-text))]
     (throw+ {:type ::malformed-log-entry :text text}
             "Log entry was malformed; could not parse."))))

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
   [#(= ::malformed-log-entry (:type %)) (fn [e] (invoke-restart :use-value "foo"))]
   (doseq [log (find-all-logs)]
     (analyze-log log))))
