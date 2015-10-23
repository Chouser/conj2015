(ns conj2015.exceptions
  (:require [clojure.java.io :as io]
            [clojure.repl :refer-all true]))

;; === basic ===

(defn ^:dynamic *malformed-log-entry-error* [info]
  (throw (ex-info (str "Log entry was malformed; could not parse.") info)))

(defn parse-log-entry [text]
  (if (well-formed-log-entry? text)
    {:successfully-parsed text} ;; or whatever; for once, ignoring the success path
    (*malformed-log-entry-error* {:text text})))

;; -- user code --

;; throws exception on bad entry
(defn parse-log-seq [text-lines]
  (for [text text-lines
        :let [entry (parse-log-entry text)]
        :when entry]
    entry))

;; skips bad entries
(defn parse-log-seq [text-lines]
  (for [text text-lines
        :let [entry (binding [*malformed-log-entry-error* (constantly nil)]
                      (parse-log-entry text))]
        :when entry]
    entry))


;; === support for inheritence-like behavior requires base fns ===

(defn ^:dynamic *error* [info]
  (throw (ex-info (str "ERROR: " (:message info)) info)))

(defn ^:dynamic *warn* [msg info]
  (println "WARNING:" msg)
  (prn info))

;; A bit like inheritence -- call the handler you want to delegate to.
(defn ^:dynamic *malformed-log-entry-error* [info]
  (*error* (assoc info :message (str "Log entry was malformed; could not parse."))))

;; If you have a more complicated default, break it out from dynamic fn

(defn reconstitute-log-entry
  "This is the default error handler for *malformed-log-entry-error*"
  [{:as info :keys [text]}]
  ;; more complicated computation using :text
  )

(def ^:dynamic *malformed-log-entry-error* reconstitute-log-entry)


;; === can build up more detailed information, such as the stack of
;; handlers invoked

(defn ^:dynamic *error* [info]
  (throw (ex-info (str "ERROR: " (:message info))
                  (update-in info [:fns] (fnil conj []) '*error*)
                  (:caused-by info))))

(defn ^:dynamic *warn* [msg info]
  (println "WARNING:" msg)
  (prn (update-in info [:fns] (fnil conj []) '*warn*)))

;; A bit like inheritence -- call the handler you want to delegate to.
(defn ^:dynamic *malformed-log-entry-error* [info]
  (*error* (-> info
               (assoc :message "Log entry was malformed; could not parse.")
               (update-in [:fns] (fnil conj []) '*malformed-log-entry-error*))))

;; skips bad entries and any other condition that defers to *error*
(defn parse-log-seq [text-lines]
  (for [text text-lines
        :let [entry (binding [*error* (constantly :foo)]
                      (parse-log-entry text))]
        :when entry]
    entry))

;; binding isn't cheap. Move it outside loops when possible, but
;; beware of lazy seqs!


;; === support for manually defering to higher-bound handlers requires capturing ===

(defn parse-log-seq [text-lines]
  (for [text text-lines
        :let [entry (let [orig *malformed-log-entry-error*]
                      (binding [*malformed-log-entry-error* (fn [info]
                                          (if (= "good enough" (:text info))
                                            {:good-enough-parsed (:text info)}
                                            (orig info)))]
                        (parse-log-entry text)))]
        :when entry]
    entry))


;; === Can be helpful to catch Java exceptions early and call
;; rebindable handlers

(defn do-some-parsing [txt] txt)

(defn ^:dynamic *file-not-found* [info]
  (*error* info))

(defn parse-file [filename]
  (do-some-parsing
   (try
     (slurp filename)
     (catch java.io.FileNotFoundException e
       (*file-not-found* {:message (.getMessage e)
                          :filename filename
                          :caused-by e})))))
