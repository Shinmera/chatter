#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defmacro with-error-logging ((err category format-string &rest args) &body body)
  `(handler-bind ((error (lambda (,err)
                           (v:warn ,category ,format-string ,@args)
                           (v:debug ,category ,err))))
     ,@body))

(defmacro with-error-handling ((err category format-string &rest args) &body body)
  `(handler-case
       (with-error-logging (,err ,category ,format-string ,@args)
         ,@body)
     (error (,err)
       (update-status (format NIL ,format-string ,@args)))))

(defun format-long-time (stamp)
  (local-time:format-timestring NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " "
                                                    (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun format-short-time (stamp)
  (local-time:format-timestring NIL stamp :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun format-time (stamp &optional (format (s-timestamp)))
  (local-time:format-timestring NIL stamp :format (parse-time-format format)))

(defun parse-time-format (format)
  (loop for char across format
        collect (case char
                  (#\Y '(:year 4))
                  (#\M '(:month 2))
                  (#\D '(:day 2))
                  (#\h '(:hour 2))
                  (#\m '(:min 2))
                  (#\s '(:sec 2))
                  (#\d :short-weekday)
                  (#\n :short-month)
                  (T char))))

(defun boot-cleanup ()
  #+sbcl (sb-ext:disable-debugger)
  (v:restart-global-controller))

(defun build-cleanup ()
  (v:remove-global-controller))

(pushnew 'build-cleanup qtools:*build-hooks*)
(pushnew 'boot-cleanup qtools:*boot-hooks*)
