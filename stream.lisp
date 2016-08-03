#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defclass twitter-stream ()
  ((main :initarg :main :accessor main)
   (thread :initform NIL :accessor thread))
  (:default-initargs
   :main (error "MAIN required.")))

(defmethod initialize-instance :after ((stream twitter-stream) &key)
  (setf (thread stream) (bt:make-thread (lambda () (process-stream stream)))))

(defmethod finalize ((stream twitter-stream))
  (setf (main stream) NIL))

(defun process-stream (stream)
  (v:info :chatter.stream "Starting twitter stream.")
  (loop while (main stream)
        do (with-error-handling (err :chatter.stream "Error in stream processing: ~a" err)
             (chirp:stream/user
              (lambda (event)
                (when (main stream)
                  (process-stream-event event stream)
                  T)))))
  (v:info :chatter.stream "Ending twitter stream."))

(defun process-stream-event (event stream)
  (v:info :chatter.stream "~a << ~a" stream event)
  (with-simple-restart (abort "Abort processing ~a" event)
    (when (typep event 'chirp:direct-message)
      (qui:with-body-in-gui ((main stream))
        (with-error-handling (err :chatter.stream "Error processing stream event: ~a" err)
          (let* ((message (message event))
                 (conversation (ensure-conversation (participant message))))
            (update-conversation conversation message)))))))
