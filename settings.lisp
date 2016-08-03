#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(define-widget color-button (QPushButton)
  ((color :initarg :color :accessor color))
  (:default-initargs :color (list 0 0 0)))

(define-initializer (color-button setup)
  (setf (q+:flat color-button) T))

(define-slot (color-button press) ()
  (declare (connected color-button (clicked)))
  (let ((color (q+:qcolordialog-get-color (q+:make-qcolor (first color) (second color) (third color)))))
    (when (q+:is-valid color)
      (setf (color color-button) (list (q+:red color) (q+:green color) (q+:blue color)))
      (q+:repaint color-button))))

(define-override (color-button paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter color-button))
                    (color (q+:make-qcolor (first color) (second color) (third color))))
    (q+:fill-rect painter (q+:rect color-button) color)))

(define-widget settings (QDialog window)
  ())

(define-subwidget (settings timestamp) (q+:make-qlineedit))

(define-subwidget (settings me-color) (make-instance 'color-button))

(define-subwidget (settings you-color) (make-instance 'color-button))

(define-subwidget (settings time-color) (make-instance 'color-button))

(define-subwidget (settings text-color) (make-instance 'color-button))

(define-subwidget (settings base-color) (make-instance 'color-button))

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
    (q+:add-row inner "Base Color:" base-color)
    (q+:add-layout layout inner 0 0 1 2))
  (q+:add-widget layout ok      1 0 1 1)
  (q+:add-widget layout cancel  1 1 1 1))
