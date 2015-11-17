(ns conj2015.n06_xc_grouping
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

;; === errors ===
(defn ^:dynamic *error* [msg info]
  (throw (ex-info msg info)))

(defn ^:dynamic *math-error* [msg info]
  (*error* msg info))

(defn ^:dynamic *sqrt-of-negative* [msg info]
  (*math-error* msg info))

(defn ^:dynamic *malformed-entry-error* [msg info]
  (*error* msg info))

(defn ^:dynamic *malformed-log-entry-error* [msg info]
  (*malformed-entry-error* msg info))

;; === restarts ===
(defn ^:dynamic *use-value* [value]
  (throw (ex-info "Restart *use-value* is unbound." {:value value})))

;; === application code ===
(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (binding [*use-value* identity]
      (*malformed-log-entry-error* "Log entry was malformed; could not parse."
                                   {:text text}))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep parse-log-entry lines)))

(defn analyze-log [log]
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (binding [*malformed-log-entry-error*
            (fn [msg info] (*use-value* {:failed-to-parse (:text info)}))]
    (doseq [log (find-all-logs)]
      (analyze-log log))))

(defn sqrt [x]
  (let [result (Math/sqrt x)]
    (if (Double/isNaN result)
      (binding [*use-value* identity]
        (*sqrt-of-negative* "Square root of a negative number"
                            {:sqrt-of x}))
      result)))

(binding [*error* (fn [msg info] (*use-value* (:sqrt-of info)))] (sqrt -2))
