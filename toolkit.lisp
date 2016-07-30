#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defmacro with-error-logging ((category format-string &rest args) &body body)
  (let ((err (gensym "ERR")))
    `(handler-bind ((error (lambda (,err)
                             (v:warn ,category ,format-string ,@args)
                             (v:debug ,category ,err))))
       ,@body)))

(defun format-long-time (stamp)
  (local-time:format-timestring NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " "
                                                    (:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun format-short-time (stamp)
  (local-time:format-timestring NIL stamp :format '((:hour 2) ":" (:min 2) ":" (:sec 2))))

(defun boot-cleanup ()
  #+sbcl (sb-ext:disable-debugger)
  (v:restart-global-controller))

(defun build-cleanup ()
  (v:remove-global-controller))

(pushnew 'build-cleanup qtools:*build-hooks*)
(pushnew 'boot-cleanup qtools:*boot-hooks*)
