#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget login (QDialog window)
  ((verifier :initform NIL :accessor verifier)
   (token :initform NIL :accessor token)
   (secret :initform NIL :accessor secret)
   (use-server :initarg :use-server :accessor use-server))
  (:default-initargs
    :use-server NIL))

(define-initializer (login restore-from-save)
  (ubiquitous:with-local-storage ('twitter-credentials)
    (setf (token login) (ubiquitous:value :token))
    (setf (secret login) (ubiquitous:value :secret)))
  (when (and (token login) (secret login))
    (signal! login (completed))))

(define-subwidget (login avatar) (make-instance 'avatar :user "You"))

(define-subwidget (login username) (q+:make-qlabel "Please Login.")
  (setf (q+:alignment username) (q+:qt.align-center)))

(define-subwidget (login button) (q+:make-qpushbutton "Login"))

(define-subwidget (login pin) (q+:make-qlineedit)
  (q+:hide pin)
  (setf (q+:placeholder-text pin) "65312.."))

(define-subwidget (login layout) (q+:make-qvboxlayout login)
  (setf (q+:window-title login) "Login to Twitter")
  (setf (q+:minimum-size login) (values 300 200))
  (q+:add-widget layout avatar)
  (q+:add-widget layout username)
  (q+:add-widget layout button)
  (q+:add-widget layout pin)
  (setf (q+:alignment layout) (q+:qt.align-center)))

(define-signal (login succeeded) ())
(define-signal (login failed) ())
(define-signal (login completed) ())

(define-slot (login start) ()
  (declare (connected button (clicked)))
  (q+:hide button)
  (cond ((use-server login)
         (let ((server (start-login-server)))
           (chirp:initiate-authentication :method (server-callback-address server))))
        (T
         (setf (q+:text username) "Enter the PIN:")
         (with-finalizing ((url (q+:make-qurl (chirp:initiate-authentication :method :pin))))
           (q+:qdesktopservices-open-url url))
         (q+:show pin)
         (q+:set-focus pin))))

(define-slot (login pin-entered) ()
  (declare (connected pin (editing-finished)))
  (let ((verifier (q+:text pin)))
    (setf (q+:text pin) "") ;; Clear to avoid accidental resignal
    (when (<= 6 (length verifier))
      (setf (verifier login) verifier)
      (setf (token login) chirp:*oauth-access-token*)
      (signal! login (succeeded)))))

(define-slot (login success) ()
  (declare (connected login (succeeded)))
  (let ((verifier (verifier login))
        (token (token login)))
    (handler-case
        (with-error-logging (:chatter.login "Login failed.")
          (multiple-value-bind (token secret) (chirp:complete-authentication verifier token)
            (setf (token login) token)
            (setf (secret login) secret)
            (signal! login (completed))))
      (error (err)
        (signal! login (failed))))))

(define-slot (login complete) ()
  (declare (connected login (completed)))
  (setf chirp:*oauth-access-token* (token login))
  (setf chirp:*oauth-access-secret* (secret login))
  (handler-case
      (with-error-logging (:chatter.login "Login completion failed.")
        (let ((self (chirp:account/self)))
          (ubiquitous:with-local-storage ('twitter-credentials)
            (setf (ubiquitous:value :token) (token login))
            (setf (ubiquitous:value :secret) (secret login)))
          (setf (q+:text username) (chirp:screen-name self))
          (setf (image avatar) self)
          (q+:hide pin)
          (q+:hide button)))
    (error (err)
      (signal! login (failed)))))

(define-slot (login failure) ()
  (declare (connected login (failed)))
  (setf (q+:text username) "Login failed.")
  (q+:hide pin)
  (q+:show button))
