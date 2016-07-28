#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *standalone* NIL)

(defun register-as-standalone ()
  (setf *standalone* T))

(push 'register-as-standalone qtools:*boot-hooks*)

(defun root ()
  (if *standalone*
      (or (first (uiop:command-line-arguments))
          *default-pathname-defaults*)
      (asdf:system-source-directory :chatter)))

(defun resource (name)
  (merge-pathnames name (merge-pathnames "static/" (root))))

(defun resource-string (name)
  (uiop:native-namestring (resource name)))
