#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *windows* (make-hash-table :test 'eql))

(defun window (name)
  (gethash name *windows*))

(defun (setf window) (window name)
  (setf (gethash name *windows*) window))

(defun remove-window (name)
  (remhash name *windows*))

(define-widget window (QWidget)
  ((name :initarg :name :reader name)))

(defmethod shared-initialize :after ((window window) slots &key)
  (unless (slot-boundp window 'name)
    (setf (slot-value window 'name)
          (class-name (class-of window))))
  (setf (window (name window)) window))

(define-finalizer (window deregister-window)
  (remove-window (name window)))
