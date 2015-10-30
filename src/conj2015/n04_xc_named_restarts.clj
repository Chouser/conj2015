(ns conj2015.n04-xc-named-restarts
  (:require [conj2015.common :refer [find-all-logs analyze-entry
                                     well-formed-log-entry?]]
            [clojure.repl :refer :all]
            [clojure.java.io :as io]))

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
  (doseq [entry (parse-log-file log)]
    (analyze-entry entry)))

(defn log-analyzer []
  (binding [*malformed-log-entry-error*
            (fn [msg info] (*use-value* {:failed-to-parse (:text info)}))]
    (doseq [log (find-all-logs)]
      (analyze-log log))))

;;             (fn [msg info] (*use-value* {:failed-to-parse (:text info)}))]
;; conj2015.n03-xc-multi-restarts> (log-analyzer)
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


;;             (fn [msg info] (*reparse-entry* (str "better than " (:text info))))]
;; conj2015.n03-xc-multi-restarts> (log-analyzer)
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

;;             #'*skip-log-entry*]
;; conj2015.n03-xc-multi-restarts> (log-analyzer)
;; {:successfully-parsed "a"}
;; {:successfully-parsed "b"}
;; {:successfully-parsed "c"}
;; {:successfully-parsed "D"}
;; {:successfully-parsed "F"}
;; {:successfully-parsed "h"}
;; {:successfully-parsed "i"}
;; nil

;; raise during handling
;; StackOverflowError   clojure.lang.PersistentHashMap$BitmapIndexedNode.index (PersistentHashMap.java:677)
