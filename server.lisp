#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *server* NIL)

(defun ensure-server (&optional port)
  (or *server*
      (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))))

(defun start-login-server (&key (port (find-port:find-port))
                                (server (ensure-server port)))
  (unless (hunchentoot:started-p server)
    (hunchentoot:start server))
  (push #'hunchentoot-dispatcher hunchentoot:*dispatch-table*)
  server)

(defun stop-login-server (&optional (server *server*))
  (when server
    (when (hunchentoot:started-p server)
      (hunchentoot:stop server))))

(defun hunchentoot-dispatcher (request)
  (flet ((ensure-value (arg)
           (and arg (string/= arg ""))))
    (when (search "login-return" (hunchentoot:script-name request))
      (lambda (&rest args)
        (declare (ignore args))
        (let ((verifier (ensure-value (hunchentoot:get-parameter "oauth_verifier")))
              (token (ensure-value (hunchentoot:get-parameter "oauth_token")))
              (login (window 'login)))
          (cond ((and verifier token)
                 (v:info :chatter.login.server "Successful login.")
                 (setf (verifier login) verifier)
                 (setf (token login) token)
                 (signal! login (succeeded)))
                (T
                 (v:info :chatter.login.server "Failed login.")
                 (signal! (window 'login) (failed))))
          (stop-login-server (hunchentoot:request-acceptor request)))))))

(defun server-callback-address (&optional (server *server*))
  (format NIL "http://localhost:~a/login-return" (hunchentoot:acceptor-port server)))
