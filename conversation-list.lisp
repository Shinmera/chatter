#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget conversation-list (QDockWidget)
  ())

(define-initializer (conversation-list setup)
  (setf (q+:features conversation-list) (q+:qdockwidget.no-dock-widget-features))
  (setf (q+:title-bar-widget conversation-list) (q+:make-qwidget conversation-list)))

(define-subwidget (conversation-list list) (q+:make-qlistwidget)
  (repopulate-conversation-list list))

(define-subwidget (conversation-list scroller) (q+:make-qscrollarea)
  (setf (q+:widget-resizable scroller) T)
  (setf (q+:widget scroller) list))

(define-subwidget (conversation-list add) (q+:make-qpushbutton "+"))

(define-subwidget (conversation-list center) (q+:make-qwidget)
  (setf (q+:widget conversation-list) center))

(define-subwidget (conversation-list layout) (q+:make-qvboxlayout center)
  (setf (q+:margin layout) 0)
  (q+:add-widget layout scroller)
  (let ((inner (q+:make-qhboxlayout)))
    (setf (q+:margin inner) 0)
    (q+:add-widget inner add)
    (q+:add-layout layout inner)))

(defmethod show-conversation ((convo conversation) (list conversation-list))
  (let ((list (slot-value list 'list)))
    (dotimes (i (q+:count list))
      (show-conversation convo (q+:item-widget list (q+:item list i))))))

(defmethod update-conversation ((convo conversation) (list conversation-list))
  ;; FIXME: More people might get added to convos, update title.
  )

(defun repopulate-conversation-list (list &optional (conversations (conversations)))
  (dotimes (i (q+:count list))
    (finalize (q+:item-widget list (q+:item list i))))
  (q+:clear list)
  (dolist (conv conversations)
    (let ((item (q+:make-qlistwidgetitem list)))
      (q+:add-item list item)
      (setf (q+:size-hint item) (q+:make-qsize 32 32))
      (setf (q+:item-widget list item) (make-instance 'conversation-item :conversation conv)))))

(define-widget conversation-item (QWidget)
  ((conversation :initarg :conversation)))

(define-subwidget (conversation-item name) (q+:make-qlabel)
  (setf (q+:text name) (label conversation)))

(define-subwidget (conversation-item avatar) NIL
  (setf avatar (copy (avatar (first (participants conversation)))))
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
  (setf (selected item) (eql convo (slot-value item 'conversation))))

(defmethod (setf selected) (value (item conversation-item))
  (setf (q+:color (q+:palette item) (q+:qpalette.window))
        (if value
            (q+:make-qcolor #x00 #x88 #xEE)
            (q+:make-qcolor 0 0 0 0))))
