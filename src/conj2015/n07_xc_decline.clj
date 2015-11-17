(ns conj2015.n07-xc-decline
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

;; === errors ===
(defn ^:dynamic *malformed-log-entry-error* [msg info]
  (throw (ex-info msg info)))

;; === restarts ===
(defn ^:dynamic *use-value* [value]
  (throw (ex-info "Restart *use-value* is unbound." {:value value})))

(defn ^:dynamic *skip-log-entry* []
  (throw (ex-info "Restart *skip-log-entry* is unbound." {})))

(defn ^:dynamic *reparse-entry* [fixed-text]
  (throw (ex-info "Restart *reparse-entry* is unbound." {})))

;; === application code ===
(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (binding [*use-value* identity
              *reparse-entry* parse-log-entry]
      (*malformed-log-entry-error* "Log entry was malformed; could not parse."
                                   {:text text}))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(binding [*skip-log-entry* (constantly nil)]
             (parse-log-entry %)) lines)))

(defn analyze-log [log]
  (let [decline-malformed-log-entry-error
          *malformed-log-entry-error*]
    (binding [*malformed-log-entry-error*
              (fn [msg {:as info :keys [text]}]
                (if (= "bad1" text)
                  (*use-value* {:bad1-is-ok text})
                  (decline-malformed-log-entry-error
                     msg info)))]
      (doseq [entry (parse-log-file log)]
        (analyze-entry entry)))))

(defn log-analyzer []
  (binding [*malformed-log-entry-error*
            (fn [msg info] {:failed-to-parse (:text info)})]
    (doseq [log (find-all-logs)]
      (analyze-log log))))

