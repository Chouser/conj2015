;; sbcl --script n03_multi_restarts.lisp  # ...or...
;; sbcl --load   n03_multi_restarts.lisp

(defvar *level* :top)

(defun find-all-logs ()
  '((a b c) (D bad1 F) (bad2 h i)))

(defun analyze-entry (entry)
  (print entry))

(defun well-formed-log-entry-p (text)
  (not (string= (format nil "~A xxx" text) "BAD" :end1 3)))

;; --- begin example ---

(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun skip-log-entry ()
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))

(defun reparse-entry (text)
  (let ((restart (find-restart 'reparse-entry)))
    (when restart (invoke-restart restart text))))

;; (error 'malformed-log-entry-error :text "greetings")
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
    `(:successfully-parsed ,text)
    (restart-case (let ((*level* :low)) (error 'malformed-log-entry-error :text text))
      (use-value (value) value)
      (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defun parse-log-file (lines)
  (loop for text in lines
        for entry = (restart-case (parse-log-entry text)
                                  (skip-log-entry () (error 'malformed-log-entry-error :text "again!") nil))
        when entry collect it))

(defun analyze-log (log)
  (let ((*level* :mid))
    (dolist (entry (parse-log-file log))
      (analyze-entry entry))))

(defun log-analyzer ()
  (let ((*level* :high))
    (handler-bind ((malformed-log-entry-error
                    #'(lambda (c)
                        (use-value *level*))))
                  (dolist (log (find-all-logs))
                    (analyze-log log)))))

(log-analyzer)

;;                         (skip-log-entry))))
;; Unhandled MALFORMED-LOG-ENTRY-ERROR in thread #<SB-THREAD:THREAD
;;                                                 "main thread" RUNNING
;;                                                  {1002A8B2E3}>:
;;   Condition MALFORMED-LOG-ENTRY-ERROR was signalled.

;;                         (use-value *level*))))
;; (:SUCCESSFULLY-PARSED A)
;; (:SUCCESSFULLY-PARSED B)
;; (:SUCCESSFULLY-PARSED C)
;; (:SUCCESSFULLY-PARSED D)
;; :LOW
;; (:SUCCESSFULLY-PARSED F)
;; :LOW
;; (:SUCCESSFULLY-PARSED H)
;; (:SUCCESSFULLY-PARSED I)
