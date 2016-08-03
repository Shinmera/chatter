#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(ubiquitous:restore 'settings)

(defmacro define-setting (name default &rest path)
  `(progn (defun ,name () (ubiquitous:defaulted-value ,default ,@path))
          (defun (setf ,name) (value) (setf (ubiquitous:value ,@path) value))))

(define-setting s-timestamp "h:m:s" :timestamp)
(define-setting s-me-color "#0088EE" :colors :me)
(define-setting s-you-color "#EE8800" :colors :you)
(define-setting s-time-color "#555555" :colors :time)
(define-setting s-text-color "#EEEEEE" :colors :text)
(define-setting s-base-color "#151515" :colors :base)
(define-setting s-link-color "#0088EE" :colors :link)

(define-widget color-button (QPushButton)
  ((color :initarg :color :accessor color))
  (:default-initargs :color "#0088EE"))

(define-initializer (color-button setup)
  (setf (q+:flat color-button) T)
  (setf (q+:maximum-height color-button) 20))

(define-slot (color-button press) ()
  (declare (connected color-button (clicked)))
  (let ((color (q+:qcolordialog-get-color (q+:make-qcolor color))))
    (when (q+:is-valid color)
      (setf (color color-button) (q+:name color))
      (q+:repaint color-button))))

(define-override (color-button paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter color-button))
                    (color (q+:make-qcolor color)))
    (q+:fill-rect painter (q+:rect color-button) color)))

(define-widget settings (QDialog window)
  ())

(define-subwidget (settings timestamp) (q+:make-qlineedit (s-timestamp)))

(define-subwidget (settings me-color) (make-instance 'color-button :color (s-me-color)))

(define-subwidget (settings you-color) (make-instance 'color-button :color (s-you-color)))

(define-subwidget (settings time-color) (make-instance 'color-button :color (s-time-color)))

(define-subwidget (settings text-color) (make-instance 'color-button :color (s-text-color)))

(define-subwidget (settings link-color) (make-instance 'color-button :color (s-link-color)))

(define-subwidget (settings base-color) (make-instance 'color-button :color (s-base-color)))

(define-subwidget (settings ok) (q+:make-qpushbutton "Ok")
  (connect! ok (clicked) settings (accept)))

(define-subwidget (settings cancel) (q+:make-qpushbutton "Cancel")
  (connect! cancel (clicked) settings (reject)))

(define-subwidget (settings layout) (q+:make-qgridlayout settings)
  (let ((inner (q+:make-qformlayout)))
    (q+:add-row inner "Timestamp Format:" timestamp)
    (q+:add-row inner "My Color:" me-color)
    (q+:add-row inner "Partner's Color:" you-color)
    (q+:add-row inner "Timestamp Color:" time-color)
    (q+:add-row inner "Text Color:" text-color)
    (q+:add-row inner "Link Color:" link-color)
    (q+:add-row inner "Base Color:" base-color)
    (q+:add-layout layout inner 0 0 1 2))
  (q+:add-widget layout ok      1 0 1 1)
  (q+:add-widget layout cancel  1 1 1 1))

(define-slot (settings accept) ()
  (setf (s-timestamp) (q+:text timestamp))
  (setf (s-me-color) (color me-color))
  (setf (s-you-color) (color you-color))
  (setf (s-time-color) (color time-color))
  (setf (s-text-color) (color text-color))
  (setf (s-link-color) (color link-color))
  (setf (s-base-color) (color base-color))
  (q+:done settings (q+:qdialog.accepted)))
