#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget main (QMainWindow window)
  ())

(define-initializer (main setup)
  (setf (q+:window-title main) "Chatter")
  (setf (q+:minimum-size main) (values 300 200)))

(define-subwidget (main chat) (make-instance 'chat)
  (setf (q+:central-widget main) chat))

(define-subwidget (main conversation-list) (make-instance 'conversation-list)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) conversation-list))

(define-subwidget (main stream) (make-instance 'twitter-stream :main main))

(defmethod show-conversation ((convo conversation) (main main))
  (show-conversation convo (slot-value main 'conversation-list))
  (show-conversation convo (slot-value main 'chat)))

(defmethod update-conversation ((convo conversation) (main main))
  (update-conversation convo (slot-value main 'conversation-list))
  (update-conversation convo (slot-value main 'chat)))

(defun start (&key skip-login)
  (let ((result (q+:qdialog.accepted)))
    (unless skip-login
      (with-main-window (login 'login :body :after-exec)
        (setf result (q+:result login))))
    (when (= result (q+:qdialog.accepted))
      (with-main-window (main 'main)))))
