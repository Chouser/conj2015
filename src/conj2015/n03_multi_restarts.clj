(ns conj2015.n03-multi-restarts
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [slingshot.core :refer [throw+]]
            [swell.api :refer [restart-case handler-bind invoke-restart]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (restart-case
     [:use-value (fn [value]
                   value)
      :reparse-entry (fn [fixed-text]
                       (parse-log-entry fixed-text))]
     (throw+ {:type ::malformed-log-entry :text text}
             (str "Log entry was malformed;"
                  " could not parse.")))))

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
      (invoke-restart
       :use-value {:failed-to-parse (:text (:object e))}))]
   (doseq [log (find-all-logs)]
     (analyze-log log))))


;;       :use-value {:failed-to-parse (:text (:object e))}
;; conj2015.n03-multi-restarts> (log-analyzer)
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

;;        :reparse-entry (str "better than " (:text (:object e)))
;; conj2015.n03-multi-restarts> (log-analyzer)
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

;;        :skip-log-entry
;; conj2015.n03-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil

;;        :no-such-restart
;; conj2015.n03-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; Stone Object thrown by throw+: {:restart :no-such-restart, :args nil}  swell.slingshot/unwind-to-invoke-restart (slingshot.clj:70)
