#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget chat (QWidget)
  ((conversation :initform NIL :accessor conversation)
   (last-message :initform NIL :accessor last-message)))

(define-subwidget (chat output) (make-instance 'chat-view))

(define-subwidget (chat input) (make-instance 'chat-input))

(define-subwidget (chat send) (q+:make-qpushbutton "Send"))

(define-subwidget (chat image) (q+:make-qpushbutton "Image"))

(define-subwidget (chat layout) (q+:make-qgridlayout chat)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 2)
  (q+:add-widget layout output 0 0 1 2)
  (q+:add-widget layout input 1 0 2 1)
  (q+:add-widget layout send 1 1 1 1)
  (q+:add-widget layout image 2 1 1 1))

(define-slot (chat send) ()
  (declare (connected send (clicked)))
  (declare (connected input (confirmed)))
  (let ((text (q+:to-plain-text input)))
    (q+:clear input)
    ;; Bad..
    (chirp:direct-messages/new text :user-id (id (first (participants conversation))))))

(defmethod show-conversation ((conv conversation) (chat chat))
  (unless (eql conv (conversation chat))
    (let ((text (slot-value chat 'output)))
      (q+:set-html text (with-output-to-string (out)
                          (for:for ((msg over (messages conv)))
                            (show-message msg out)
                            (setf (last-message chat) msg))))
      (setf (conversation chat) conv)
      (q+:move-cursor text (q+:qtextcursor.end))
      (q+:ensure-cursor-visible text)
      (q+:set-focus (slot-value chat 'input)))))

(defmethod update-conversation ((conv conversation) (chat chat))
  (when (eql conv (conversation chat))
    (for:for ((msg over (messages conv))
              (msgs when (< (id (last-message chat)) (id msg)) collecting msg))
      (returning (show-message msgs chat)))))

(define-widget chat-input (QTextEdit)
  ())

(define-signal (chat-input confirmed) ())

(define-override (chat-input key-press-event) (ev)
  (when (and (enum-equal (q+:key ev) (q+:qt.key_return))
             (enum-equal (q+:modifiers ev) (q+:qt.control-modifier)))
    (signal! chat-input (confirmed)))
  (stop-overriding))

(define-widget chat-view (QTextBrowser)
  ())

(define-initializer (chat-view setup)
  (setf (q+:default-style-sheet (q+:document chat-view)) "a{color:#0088EE;text-decoration: none}")
  (setf (q+:text-interaction-flags chat-view) (q+:qt.links-accessible-by-mouse))
  (setf (q+:open-external-links chat-view) T)
  (setf (q+:read-only chat-view) T))

(define-subwidget (chat-view font) (q+:make-qfont "Consolas, Inconsolata, Monospace" 10)
  (setf (q+:style-hint font) (q+:qfont.type-writer))
  (setf (q+:font chat-view) font))

(defmethod show-message ((message message) (stream stream))
  (format stream "<span style=\"color:gray\">~a</span> ~
                  <span style=\"color:~a\">~a:</span> ~
                  <span style=\"color:white;display: inline-block\">~a</span><br>"
          (format-long-time (timestamp message))
          (if (eql (sender message) (self)) "#0088EE" "#EE8800")
          (name (sender message))
          (cl-ppcre:regex-replace-all "\\n" (text message) "<br>")))

(defmethod show-message ((message message) (chat chat))
  (q+:insert-html (slot-value chat 'output)
                  (with-output-to-string (out)
                    (show-message message out)))
  (setf (last-message chat) message))

(defmethod show-message ((messages list) (chat chat))
  (q+:insert-html (slot-value chat 'output)
                  (with-output-to-string (out)
                    (dolist (msg messages)
                      (show-message msg out)
                      (setf (last-message chat) msg)))))
