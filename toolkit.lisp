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
