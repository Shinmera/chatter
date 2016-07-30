#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defparameter *imgur/image* "https://api.imgur.com/3/image.json")
(defvar *imgur-client-id* NIL)
(defvar *imgur-client-secret* NIL)

(defun read-imgur-application-keys (&optional (file (resource "imgur-application-keys.txt")))
  (with-open-file (stream file :direction :input)
    (setf *imgur-client-id* (read-line stream))
    (setf *imgur-client-secret* (read-line stream))))

(read-imgur-application-keys)

(defun upload-image (file)
  (let* ((raw (dexador:post *imgur/image*
                            :content `(("image" . ,file))
                            :headers `(("Authorization" . ,(format NIL "Client-ID ~a" *imgur-client-id*)))))
         (json (yason:parse raw :object-as :alist :object-key-fn #'chirp::to-keyword)))
    (flet ((p (attr o) (cdr (assoc attr o))))
      (unless (p :success json)
        (error "Failed to upload image."))
      (let ((data (p :data json)))
        (values (p :link data)
                (p :id data)
                (p :deletehash data))))))
