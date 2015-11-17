(ns conj2015.n08-xc-signal-v-error
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(defn ^:dynamic *base-error* [msg info]
  (throw (ex-info msg info)))

(defn signal [unhandled-value error-fn msg info]
  (binding [*base-error* (constantly unhandled-value)]
    (error-fn msg info)))

;; === errors ===
(defn ^:dynamic *error* [msg info]
  (*base-error* msg info))

(defn ^:dynamic *malformed-entry-error* [msg info]
  (*error* msg info))

(defn ^:dynamic *malformed-log-entry-error* [msg info]
  (*malformed-entry-error* msg info))

(defn ^:dynamic *math-error* [msg info]
  (*error* msg info))

(defn ^:dynamic *sqrt-of-negative* [msg info]
  (*math-error* msg info))

;; === restarts ===
(defn ^:dynamic *use-value* [value]
  (throw (ex-info "Restart *use-value* is unbound." {:value value})))

;; === application code ===
(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (binding [*use-value* identity]
      (signal {:no-handler-found text}
              *malformed-log-entry-error*
              "Log entry was malformed; could not parse."
              {:text text}))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(parse-log-entry %) lines)))

(defn analyze-log [log]
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (doseq [log (find-all-logs)]
    (analyze-log log)))

;; conj2015.n08-xc-signal-v-error> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:no-handler-found "bad1"}
;; {:successfully-parsed "F"}
;; {:no-handler-found "bad2"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil
