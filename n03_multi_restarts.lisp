;; sbcl --script n03_multi_restarts.lisp  # ...or...
;; sbcl --load   n03_multi_restarts.lisp

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
    (restart-case (error 'malformed-log-entry-error :text text)
      (use-value (value) value)
      (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

(defun parse-log-file (lines)
  (loop for text in lines
        for entry = (restart-case (parse-log-entry text)
                                  (skip-log-entry () nil))
        when entry collect it))

(defun analyze-log (log)
  (dolist (entry (parse-log-file log))
    (analyze-entry entry)))

(defun log-analyzer ()
  (dolist (log (find-all-logs))
    (analyze-log log)))

(log-analyzer)

;;                      (use-value `(:failed-to-parse ,(text e))))))
;; (:SUCCESSFULLY-PARSED A) 
;; (:SUCCESSFULLY-PARSED B) 
;; (:SUCCESSFULLY-PARSED C) 
;; (:SUCCESSFULLY-PARSED D) 
;; (:FAILED-TO-PARSE TEXT) 
;; (:SUCCESSFULLY-PARSED F) 
;; (:FAILED-TO-PARSE TEXT) 
;; (:SUCCESSFULLY-PARSED H) 
;; (:SUCCESSFULLY-PARSED I)

;;                       (reparse-entry (format nil "better than ~A" (text e))))))
;; (:SUCCESSFULLY-PARSED A) 
;; (:SUCCESSFULLY-PARSED B) 
;; (:SUCCESSFULLY-PARSED C) 
;; (:SUCCESSFULLY-PARSED D) 
;; (:SUCCESSFULLY-PARSED "better than BAD1") 
;; (:SUCCESSFULLY-PARSED F) 
;; (:SUCCESSFULLY-PARSED "better than BAD2") 
;; (:SUCCESSFULLY-PARSED H) 
;; (:SUCCESSFULLY-PARSED I)

;;                       (skip-log-entry))))
;; (:SUCCESSFULLY-PARSED A) 
;; (:SUCCESSFULLY-PARSED B) 
;; (:SUCCESSFULLY-PARSED C) 
;; (:SUCCESSFULLY-PARSED D) 
;; (:SUCCESSFULLY-PARSED F) 
;; (:SUCCESSFULLY-PARSED H) 
;; (:SUCCESSFULLY-PARSED I)

