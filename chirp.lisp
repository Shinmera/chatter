#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defun read-twitter-application-keys (&optional (file (resource "twitter-application-keys.txt")))
  (with-open-file (stream file :direction :input)
    (setf chirp:*oauth-api-key* (read-line stream))
    (setf chirp:*oauth-api-secret* (read-line stream))))

(read-twitter-application-keys)
