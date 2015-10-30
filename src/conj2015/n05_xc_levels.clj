(ns conj2015.n05-xc-levels
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

(def ^:dynamic *level* :top)

;; === errors ===
(defn ^:dynamic *malformed-log-entry-error* [msg info]
  (throw (ex-info msg info)))

;; === restarts ===
(defn ^:dynamic *use-value* [value]
  (throw (ex-info "Restart *use-value* is unbound.")))

(defn ^:dynamic *skip-log-entry* []
  (throw (ex-info "Restart *skip-log-entry* is unbound.")))

(defn ^:dynamic *reparse-entry* [fixed-text]
  (throw (ex-info "Restart *reparse-entry* is unbound.")))

;; === application code ===
(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text}
    (binding [*level* :low
              *use-value* identity
              *reparse-entry* parse-log-entry]
      (*malformed-log-entry-error* "Log entry was malformed; could not parse."
                                   {:text text}))))

(defn parse-log-file [log]
  (let [lines (with-open [stream (io/reader log)]
                (doall (line-seq stream)))]
    (keep #(binding [*skip-log-entry*
                     (fn [] (*malformed-log-entry-error* "msg" {:text "again!"}))]
             (parse-log-entry %)) lines)))

(defn analyze-log [log]
  (binding [*level* :mid]
    (doseq [entry (parse-log-file log)]
      (analyze-entry entry))))

(defn log-analyzer []
  (binding [*level* :high
            *malformed-log-entry-error*
            (fn [msg info] (*use-value* *level*))]
    (doseq [log (find-all-logs)]
      (analyze-log log))))

;;             (fn [msg info] (*skip-log-entry*))]
;; StackOverflowError   clojure.lang.PersistentHashMap$BitmapIndexedNode.index (PersistentHashMap.java:677)

;;             (fn [msg info] (*use-value* *level*))]
;; conj2015.n05-xc-levels> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; :low
;; {:successfully-parsed "F"}
;; :low
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil
