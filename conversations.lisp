#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.chatter)
(in-readtable :qtools)

(defvar *self* NIL)
(defvar *users* (make-hash-table :test 'equalp))
(defvar *conversations* (make-hash-table :test 'eql))

(defun self ()
  (user (or *self* (setf *self* (chirp:id (chirp:account/self))))))

(defclass user ()
  ((id :initarg :id :accessor id)
   (name :initarg :name :accessor name)
   (avatar :initform NIL :accessor avatar))
  (:default-initargs
   :id (error "ID required.")
   :name (error "NAME required.")
   :avatar NIL))

(defmethod initialize-instance :after ((user user) &key avatar)
  (setf (avatar user) (make-instance 'avatar :image avatar)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a #~a" (name user) (id user))))

(defmethod user ((user user))
  user)

(defmethod user ((name string))
  (or (gethash name *users*)
      (user (chirp:users/show :screen-name name))))

(defmethod user ((id integer))
  (or (gethash id *users*)
      (user (chirp:users/show :user-id id))))

(defmethod user ((user chirp:user))
  (or (gethash (chirp:id user) *users*)
      (let ((user (make-instance 'user :id (chirp:id user)
                                       :name (chirp:screen-name user)
                                       :avatar user)))
        (setf (gethash (id user) *users*) user)
        (setf (gethash (name user) *users*) user))))

(defclass message ()
  ((id :initarg :id :accessor id)
   (sender :initarg :sender :accessor sender)
   (recipient :initarg :recipient :accessor recipient)
   (text :initarg :text :accessor text)
   (timestamp :initarg :timestamp :accessor timestamp))
  (:default-initargs
   :id NIL
   :sender (self)
   :recipient (error "RECIPIENT required.")
   :text (error "TEXT required.")
   :timestamp (local-time:now)))

(defmethod print-object ((msg message) stream)
  (print-unreadable-object (msg stream :type T)
    (format stream "~a ~a => ~a #~a"
            (format-long-time (timestamp msg)) (name (sender msg)) (name (recipient msg)) (id msg))))

(defmethod message ((message message))
  message)

(defmethod message ((message chirp:direct-message))
  (make-instance 'message :id (chirp:id message)
                          :sender (user (chirp:sender message))
                          :recipient (user (chirp:recipient message))
                          :text (chirp:text-with-markup message)
                          :timestamp (chirp:created-at message)))

(defmethod conversation ((message message))
  (let ((party (if (eql (sender message) (self))
                   (recipient message)
                   (sender message))))
    (conversation (id party))))

(defclass conversation ()
  ((id :initarg :id :accessor id)
   (participants :initarg :participants :accessor participants)
   (messages :initarg :messages :accessor messages))
  (:default-initargs
   :id NIL
   :participants (error "PARTICIPANTS required.")
   :messages ()))

(defmethod initialize-instance :after ((conv conversation) &key id participants)
  (unless id (setf (id conv) (id (first participants))))
  (setf (gethash (id conv) *conversations*) conv))

(defmethod print-object ((conv conversation) stream)
  (print-unreadable-object (conv stream :type T)
    (write-string (label conv) stream)))

(defmethod label ((conv conversation))
  (format NIL "~{~a~^, ~}" (mapcar #'name (participants conv))))

(defun find-participants (msgs &optional (self (self)))
  (let ((parts ()))
    (dolist (msg msgs parts)
      (unless (eql self (sender msg)) (pushnew (sender msg) parts))
      (unless (eql self (recipient msg)) (pushnew (recipient msg) parts)))))

(defmethod conversation ((conversation conversation))
  conversation)

(defmethod conversation ((id integer))
  (gethash id *conversations*))

(defmethod conversation ((msgs list))
  (let ((messages (sort (mapcar #'message msgs) #'> :key #'id)))
    (make-instance 'conversation :messages messages
                                 :participants (find-participants messages))))

(defun conversations ()
  (sort (alexandria:hash-table-values *conversations*)
        #'> :key (lambda (c) (let ((last (car (last (messages c)))))
                               (if last (id last) most-positive-fixnum)))))

(defun fetch-paged (endpoint &key (per-request 200) since upto)
  (loop for max-id = upto then next-id
        for set = (funcall endpoint :count per-request :since-id since :max-id max-id)
        for next-id = (chirp:id (car (last set)))
        while (and set (or (not max-id) (/= max-id next-id)))
        append set))

(defun fetch-direct-messages (&key (since 0) (per-request 100))
  (nconc (fetch-paged #'chirp:direct-messages :since since :per-request per-request)
         (fetch-paged #'chirp:direct-messages/sent :since since :per-request per-request)))

(defun split-direct-conversations (dms &optional (self (self)))
  (let ((map (make-hash-table :test 'eql))
        (self (id self)))
    (dolist (dm dms (alexandria:hash-table-values map))
      (let ((sender (chirp:id (chirp:sender dm)))
            (recipient (chirp:id (chirp:recipient dm))))
        (cond ((not (eql self sender))
               (push dm (gethash sender map)))
              ((not (eql self recipient))
               (push dm (gethash recipient map)))
              (T
               (push dm (gethash self map))))))))

(defun fetch-direct-conversations (&key (self (self)) since)
  (mapcar #'conversation (split-direct-conversations (fetch-direct-messages :since since) self)))
