#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *avatars* (make-hash-table :test 'equalp))

(defun coerce-user-id (user)
  (etypecase user
    (string user)
    (chirp:user (chirp:id user))))

(defun avatar (user)
  (gethash (coerce-user-id user) *avatars*))

(defun (setf avatar) (avatar user)
  (setf (gethash (coerce-user-id user) *avatars*) avatar))

(defun remove-avatar (user)
  (finalize (avatar user))
  (remhash (coerce-user-id user) *avatars*))

(define-widget avatar (QLabel)
  ((user :accessor user)
   (size :accessor size))
  (:default-initargs
    :user (error "USER required.")
    :size 64
    :image NIL))

(define-signal (avatar update-pixmap) ("QByteArray*"))

(define-slot (avatar update-pixmap) ((bytes "QByteArray*"))
  (declare (connected avatar (update-pixmap "QByteArray*")))
  (with-finalizing ((bytes bytes)
                    (pixmap (q+:make-qpixmap)))
    (q+:load-from-data pixmap bytes)
    (setf (image avatar) pixmap)))

(defmethod initialize-instance :after ((avatar avatar) &key user size image)
  (setf (q+:scaled-contents avatar) T)
  (setf (q+:alignment avatar) (q+:qt.align-center))
  (setf (user avatar) (coerce-user-id user))
  (setf (size avatar) size)
  (setf (image avatar) image)
  (setf (avatar (user avatar)) avatar))

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

(defmethod (setf image) ((object qobject) (avatar avatar))
  (qtypecase object
    (QByteArray (signal! avatar (update-pixmap "QByteArray*") object))
    (QPixmap (unless (q+:is-null object)
               (setf (q+:pixmap avatar) object)
               (q+:update avatar (q+:rect avatar))))))
