#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget main (QMainWindow window qui:executable)
  ())

(define-initializer (main setup)
  (setf (q+:window-title main) "Chatter")
  (setf (q+:minimum-size main) (values 300 200)))

(define-subwidget (main chat) (make-instance 'chat)
  (setf (q+:central-widget main) chat))

(define-subwidget (main conversation-list) (make-instance 'conversation-list)
  (q+:add-dock-widget main (q+:qt.left-dock-widget-area) conversation-list))

(define-subwidget (main stream) (make-instance 'twitter-stream :main main))

(define-subwidget (main update-timer) (q+:make-qtimer)
  (setf (q+:single-shot update-timer) T)
  (q+:start update-timer 1))

(define-subwidget (main status) (q+:make-qlabel "Hello.")
  (q+:add-permanent-widget (q+:status-bar main) status))

(define-slot (main update-dms) ()
  (declare (connected update-timer (timeout)))
  (update-status "Polling for direct messages..." main)
  (bt:make-thread
   (lambda ()
     (update-direct-conversations)
     (qui:with-body-in-gui (main)
       (update-status "" main)
       (q+:start update-timer (* 1000 60))))
   :name "direct-message update thread"))

(define-override (main resize-event) (ev)
  (update-chat-cursor chat)
  (stop-overriding))

(define-menu (main File)
  (:item "Logout"
         (ubiquitous:destroy 'twitter-credentials))
  (:item ("Quit" (ctrl q))
         (q+:close main)))

(defun system-about ()
  (let ((system (asdf:find-system :chatter)))
    (format NIL "~a<br />
The source code is openly available and licensed under the ~a license.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a"
            (asdf:system-description system)
            (asdf:system-license system)
            (asdf:system-homepage system)
            (asdf:system-author system)
            (asdf:component-version system))))

(define-menu (main Help)
  (:item "About"
         (with-finalizing ((box (q+:make-qmessagebox main)))
           (setf (q+:window-title box) "About Chatter")
           (setf (q+:text box) (system-about))
           (#_exec box))))

(defmethod show-conversation ((convo conversation) (main main))
  (qui:with-body-in-gui (main)
    (show-conversation convo (slot-value main 'conversation-list))
    (show-conversation convo (slot-value main 'chat))))

(defmethod update-conversation ((convo conversation) (main main))
  (qui:with-body-in-gui (main)
    (update-conversation convo (slot-value main 'conversation-list))
    (update-conversation convo (slot-value main 'chat))))

(defun update-status (text &optional (main (window 'main)))
  (when main
    (qui:with-body-in-gui (main)
      (setf (q+:text (slot-value main 'status)) text))))

(defun start (&key skip-login)
  (when (or skip-login (login))
    (with-main-window (main 'main))))
