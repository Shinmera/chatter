#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget chat (QWidget)
  ((conversation :initform NIL :accessor conversation)
   (last-message :initform NIL :accessor last-message)
   (selected-file :initform NIL :accessor selected-file)))

(define-subwidget (chat output) (make-instance 'chat-view))

(define-subwidget (chat input) (make-instance 'chat-input))

(define-subwidget (chat left) (q+:make-qlabel)
  (setf (q+:text left) (prin1-to-string (chirp:dm-text-character-limit (chirp:help/configuration))))
  (setf (q+:size-policy left) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum))))

(define-subwidget (chat send) (q+:make-qpushbutton "Send"))

(define-subwidget (chat image) (q+:make-qpushbutton "Image"))

(define-subwidget (chat preview) (q+:make-qpushbutton)
  (q+:hide preview)
  (setf (q+:tool-tip preview) "Click to remove.")
  (setf (q+:flat preview) T)
  (setf (q+:icon-size preview) (q+:make-qsize 64 64))
  (setf (q+:style-sheet preview) "padding: 0px;"))

(define-subwidget (chat layout) (q+:make-qgridlayout chat)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 2)
  (q+:add-widget layout output  0 0 1 3)
  (q+:add-widget layout input   1 0 3 1)
  (q+:add-widget layout preview 1 1 3 1)
  (q+:add-widget layout left    1 2 1 1)
  (q+:add-widget layout send    2 2 1 1)
  (q+:add-widget layout image   3 2 1 1))

(define-slot (chat send) ()
  (declare (connected send (clicked)))
  (declare (connected input (confirmed)))
  (when conversation
    (handler-case
        (with-error-logging (:chatter.chat "Failed to send message.")
          (when (< (parse-integer (q+:text left)) 0)
            (error "Your text is too long!"))
          (let ((text (q+:to-plain-text input)))
            (when selected-file
              (setf text (format NIL "~a ~a" text (upload-image selected-file)))
              (setf selected-file NIL)
              (q+:hide preview))
            (when (= 0 (length text))
              (error "There's no text."))
            (q+:clear input)
            (reply conversation text)
            (update-chat-cursor chat)))
      (error (err)
        (update-status (format NIL "Failed to send message: ~a" err))))))

(define-slot (chat attach) ()
  (declare (connected image (clicked)))
  (with-finalizing ((dialog (q+:make-qfiledialog)))
    (setf (q+:accept-mode dialog) (q+:qfiledialog.accept-open))
    (setf (q+:file-mode dialog) (q+:qfiledialog.existing-file))
    (setf (q+:name-filter dialog) "Image files (*.png *.jpg *.jpeg *.gif)")
    (when (q+:exec dialog)
      (let ((file (first (q+:selected-files dialog))))
        (when file
          (setf selected-file (uiop:parse-native-namestring file))
          (setf (q+:icon preview) (q+:make-qicon file))
          (q+:show preview))))))

(define-slot (chat detach) ()
  (declare (connected preview (clicked)))
  (q+:hide preview)
  (setf selected-file NIL))

(define-slot (chat update-left) ()
  (declare (connected input (text-changed)))
  (declare (connected image (clicked)))
  (declare (connected preview (clicked)))
  (let ((limit (chirp:dm-text-character-limit (chirp:help/configuration)))
        (count (1- (q+:character-count (q+:document input)))))
    (when selected-file
      (incf count (1+ (chirp:short-url-length (chirp:help/configuration)))))
    (setf (q+:text left) (prin1-to-string (- limit count)))))

(defun update-chat-cursor (chat)
  (q+:move-cursor (slot-value chat 'output) (q+:qtextcursor.end))
  (q+:ensure-cursor-visible (slot-value chat 'output))
  (q+:set-focus (slot-value chat 'input)))

(defmethod show-conversation ((conv conversation) (chat chat))
  (unless (eql conv (conversation chat))
    (let ((text (slot-value chat 'output)))
      (q+:set-html text (with-output-to-string (out)
                          (for:for ((msg over (messages conv)))
                            (show-message msg out)
                            (setf (last-message chat) msg))))
      (setf (conversation chat) conv)
      (update-chat-cursor chat))))

(defmethod update-conversation ((conv conversation) (chat chat))
  (when (eql conv (conversation chat))
    (for:for ((msg over (messages conv))
              (msgs when (< (id (last-message chat)) (id msg)) collecting msg))
      (returning (show-message msgs chat)))))

(define-widget chat-input (QPlainTextEdit)
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
  (setf (last-message chat) message)
  (update-chat-cursor chat))

(defmethod show-message ((messages list) (chat chat))
  (q+:insert-html (slot-value chat 'output)
                  (with-output-to-string (out)
                    (dolist (msg messages)
                      (show-message msg out)
                      (setf (last-message chat) msg))))
  (update-chat-cursor chat))
