#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *resource-loader* NIL)
(defvar *resource-loader-thread* NIL)

(defun resource-loader ()
  (or *resource-loader*
      (let ((runner (make-instance 'simple-tasks:queued-runner)))
        (setf *resource-loader-thread* (simple-tasks:make-runner-thread runner))
        (setf *resource-loader* runner))))

(defclass resource-result ()
  ((status :initarg :status :accessor status)
   (data :initarg :data :accessor data)
   (environment :initarg :environment :accessor environment))
  (:default-initargs
   :status :ok
   :data NIL
   :environment NIL))

(defclass resource-task (simple-tasks:task)
  ((callback :initarg :callback :accessor callback))
  (:default-initargs
   :callback (error "CALLBACK required.")))

(defmethod simple-tasks:run-task :around ((task resource-task))
  (funcall (callback task)
           (restart-case
               (handler-bind ((error (lambda (err)
                                       (invoke-restart 'fail (dissect:capture-environment err)))))
                 (let ((data (call-next-method)))
                   (v:info :chatter.network "Resource request succeeded.")
                   (make-instance 'resource-result :status :ok :data data)))
             (fail (environment)
               (v:warn :chatter.network "Resource request failed: ~a" (dissect:present environment NIL))
               (make-instance 'resource-result :status :error :environment environment)))))

(defclass stream-resource-task (resource-task)
  ((address :initarg :address :accessor address)
   (element-type :initarg :element-type :accessor element-type))
  (:default-initargs
   :address (error "ADDRESS required.")
   :element-type 'character))

(defmethod read-resource-stream ((task stream-resource-task) stream)
  (cond 
    ((equal 'character (element-type task))
     (alexandria:read-stream-content-into-string stream))
    ((equal '(unsigned-byte 8) (element-type task))
     (alexandria:read-stream-content-into-byte-vector stream))))

(defclass network-task (stream-resource-task)
  ())

(defmethod simple-tasks:run-task ((task network-task))
  (multiple-value-bind (stream status) (drakma:http-request (address task) :want-stream T :close NIL)
    (unwind-protect
         (case status
           (200 (read-resource-stream task stream))
           (T (error "Bad status code: ~a" status)))
      (close stream))))

(defclass pathname-task (stream-resource-task)
  ())

(defmethod simple-tasks:run-task ((task pathname-task))
  (with-open-file (stream (address task) :direction :input :element-type (element-type task))
    (read-resource-stream task stream)))

(defmethod call-with-resource :before (resource func &key)
  (declare (ignore loader))
  (v:info :chatter.network "Requesting resource ~s..." resource))

(defmethod call-with-resource ((resource string) func &key (element-type 'character) (resource-loader (resource-loader)))
  (simple-tasks:schedule-task (make-instance 'network-task :address resource :element-type element-type :callback func) resource-loader))

(defmethod call-with-resource ((resource pathname) func &key (element-type 'character) (resource-loader (resource-loader)))
  (simple-tasks:schedule-task (make-instance 'pathname-task :address resource :element-type element-type :callback func) resource-loader))

(defmacro with-resource ((result resource &rest args) &body body)
  `(call-with-resource ,resource (lambda (,result) ,@body) ,@args))
