#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget avatar (QLabel)
  ((size :accessor size))
  (:default-initargs
    :size 64
    :image NIL))

(defmethod initialize-instance :after ((avatar avatar) &key size image)
  (setf (q+:scaled-contents avatar) T)
  (setf (q+:alignment avatar) (q+:qt.align-center))
  (setf (size avatar) size)
  (setf (image avatar) image))

(define-signal (avatar update-pixmap) ("QByteArray*"))

(define-slot (avatar update-pixmap) ((bytes "QByteArray*"))
  (declare (connected avatar (update-pixmap "QByteArray*")))
  (with-finalizing ((bytes bytes)
                    (pixmap (q+:make-qpixmap)))
    (q+:load-from-data pixmap bytes)
    (setf (image avatar) pixmap)))

(defmethod (setf size) :after (size (avatar avatar))
  (setf (q+:fixed-size avatar) (values size size)))

(defmethod (setf image) ((image null) (avatar avatar))
  (setf (image avatar) (resource "anon.png")))

(defmethod (setf image) ((user chirp:user) (avatar avatar))
  (let ((url (cdr (assoc :image-url (chirp:avatar user)))))
    (setf (image avatar) (cl-ppcre:regex-replace "_normal\\." url  "."))))

(defmethod (setf image) ((url string) (avatar avatar))
  (with-resource (reply url :element-type '(unsigned-byte 8))
    (when (eql :ok (status reply))
      (setf (image avatar) (to-qbyte-array (data reply))))))

(defmethod (setf image) ((file pathname) (avatar avatar))
  (with-resource (reply file :element-type '(unsigned-byte 8))
    (when (eql :ok (status reply))
      (setf (image avatar) (to-qbyte-array (data reply))))))

(defmethod (setf image) ((object null-qobject) (avatar avatar))
  (setf (image avatar) NIL))

(defmethod (setf image) ((object qobject) (avatar avatar))
  (qtypecase object
    (QByteArray (signal! avatar (update-pixmap "QByteArray*") object))
    (QPixmap (unless (q+:is-null object)
               (setf (q+:pixmap avatar) object)
               (q+:update avatar (q+:rect avatar))))))

(defmethod copy ((avatar avatar))
  (make-instance 'avatar :size (size avatar) :image (q+:pixmap avatar)))
