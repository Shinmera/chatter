#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget conversation-list (QDockWidget)
  ())

(defmethod construct ((conversation-list conversation-list))
  (new conversation-list "Conversations"))

(define-initializer (conversation-list setup)
  (setf (q+:features conversation-list) (q+:qdockwidget.no-dock-widget-features))
  (setf (q+:title-bar-widget conversation-list) (q+:make-qwidget conversation-list)))

(define-subwidget (conversation-list list) (q+:make-qlistwidget)
  (repopulate-conversation-list list))

(define-subwidget (conversation-list scroller) (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (conversation-list add) (q+:make-qpushbutton "+"))

(define-subwidget (conversation-list username) (q+:make-qlineedit)
  (setf (q+:placeholder-text username) "Twitter name.."))

(define-subwidget (conversation-list center) (q+:make-qwidget)
  (setf (q+:widget conversation-list) center)
  (setf (q+:size-policy center) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum))))

(define-subwidget (conversation-list layout) (q+:make-qgridlayout center)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout scroller 0 0 1 2)
  (q+:add-widget layout username 1 0 1 1)
  (q+:add-widget layout add 1 1 1 1))

(define-slot (conversation-list add) ()
  (declare (connected add (clicked)))
  (declare (connected username (return-pressed)))
  (let ((name (string-trim " " (q+:text username))))
    (with-error-handling (err :chatter.conersation-list "Failed to add conversation: ~a" err)
      (setf (q+:text username) "")
      (when (string/= name "")
        (show-conversation (ensure-conversation name) (window 'main))))))

(defmethod show-conversation ((convo conversation) (list conversation-list))
  (let ((list (slot-value list 'list)))
    (dotimes (i (q+:count list))
      (show-conversation convo (q+:item-widget list (q+:item list i))))))

(defun add-conversation-to-list (conv list)
  (let ((item (q+:make-qlistwidgetitem list)))
    (q+:add-item list item)
    (setf (q+:size-hint item) (q+:make-qsize 32 32))
    (setf (q+:item-widget list item) (make-instance 'conversation-item :conversation conv))))

(defmethod update-conversation ((convo conversation) (list conversation-list))
  (let ((list (slot-value list 'list)))
    (loop for i from 0 below (q+:count list)
          for widget = (q+:item-widget list (q+:item list i))
          do (when (eql convo (conversation widget))
               (update-conversation convo widget)
               (return))
          finally (add-conversation-to-list convo list))))

(defun repopulate-conversation-list (list &optional (conversations (conversations T)))
  (dotimes (i (q+:count list))
    (finalize (q+:item-widget list (q+:item list i))))
  (q+:clear list)
  (dolist (conv conversations)
    (add-conversation-to-list conv list)))

(define-widget conversation-item (QWidget)
  ((conversation :initarg :conversation :accessor conversation)))

(define-subwidget (conversation-item name) (q+:make-qlabel)
  (setf (q+:text name) (label conversation)))

(define-subwidget (conversation-item avatar) NIL
  (setf avatar (copy (avatar conversation)))
  (setf (size avatar) 32))

(define-subwidget (conversation-item layout) (q+:make-qhboxlayout conversation-item)
  (setf (q+:auto-fill-background conversation-item) T)
  (setf (q+:margin layout) 1)
  (q+:add-widget layout avatar)
  (q+:add-widget layout name 1))

(define-override (conversation-item mouse-release-event) (ev)
  (when (enum-equal (q+:button ev) (q+:qt.left-button))
    (show-conversation conversation (window 'main))))

(defmethod show-conversation ((convo conversation) (item conversation-item))
  (setf (selected item) (eql convo (conversation item))))

(defmethod update-conversation ((convo conversation) (item conversation-item))
  (setf (q+:text (slot-value item 'name)) (label convo)))

(defmethod (setf selected) (value (item conversation-item))
  ;; (setf (q+:color (q+:palette item) (q+:qpalette.window))
  ;;       (if value
  ;;           (q+:make-qcolor #x00 #x88 #xEE)
  ;;           (q+:make-qcolor 0 0 0 0)))
  )
