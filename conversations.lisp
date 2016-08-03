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
  (or *self* (setf *self* (user (chirp:account/self)))))

(defclass user ()
  ((id :initarg :id :accessor id)
   (name :initarg :name :accessor name)
   (real-name :initform NIL :accessor real-name)
   (description :initarg :description :accessor description)
   (url :initarg :url :accessor url)
   (avatar :initform NIL :accessor avatar))
  (:default-initargs
   :id (error "ID required.")
   :name (error "NAME required.")
   :description NIL
   :url NIL
   :avatar NIL))

(defmethod initialize-instance :after ((user user) &key avatar real-name)
  (setf (avatar user) (make-instance 'avatar :image avatar))
  (setf (real-name user) (or real-name (name user))))

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

(defun expanded-description (user)
  ;; I think the only thing that can be included are URLs so we simulate the same entity structure as with statuses.
  (chirp:text-with-markup `((:urls .,(second (assoc :description (chirp:entities user))))) :text (chirp:description user)))

(defmethod user ((user chirp:user))
  (or (gethash (chirp:id user) *users*)
      ;; Fixup for entites
      (let ((user (make-instance 'user :id (chirp:id user)
                                       :name (chirp:screen-name user)
                                       :real-name (chirp:name user)
                                       :description (expanded-description user)
                                       :url (chirp:url user)
                                       :avatar user)))
        (setf (gethash (id user) *users*) user)
        (setf (gethash (name user) *users*) user))))

(defmethod conversation ((user user))
  (gethash (id user) *conversations*))

(defmethod conversations ((user user))
  (for:for ((conv table-values *conversations*)
            (convs when (find user (participants conv)) collecting conv))))

(defmethod reply ((user user) text)
  (reply (conversation user) text))

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

(defmethod participant ((message message))
  (if (eql (sender message) (self))
      (recipient message)
      (sender message)))

(defmethod conversation ((message message))
  (conversation (id (participant message))))

(defmethod reply ((message message) text)
  (reply (conversation (sender message)) text))

(defclass conversation ()
  ((id :initarg :id :accessor id)
   (messages :initform (make-array 0 :adjustable T :fill-pointer T) :accessor messages)))

(defmethod initialize-instance :after ((conv conversation) &key messages)
  (assert (not (null (id conv))) ((id conv)) "Conversation ID cannot be null.")
  (setf (gethash (id conv) *conversations*) conv)
  (update-conversation conv messages))

(defmethod print-object ((conv conversation) stream)
  (print-unreadable-object (conv stream :type T)
    (write-string (label conv) stream)))

(defmethod conversation ((conversation conversation))
  conversation)

(defmethod conversation ((id integer))
  (gethash id *conversations*))

(defmethod ensure-conversation (id)
  (conversation id))

(defmethod conversations ((everything (eql T)))
  (sort (alexandria:hash-table-values *conversations*)
        #'> :key (lambda (c) (if (= 0 (length (messages c)))
                                 most-positive-fixnum
                                 (id (aref (messages c) (1- (length (messages c)))))))))

(defmethod label ((conversation conversation))
  (id conversation))

(defmethod update-conversation ((conv conversation) (message message))
  (let ((messages (messages conv)))
    (unless (find (id message) messages :key #'id)
      (vector-push-extend message messages)
      ;; If we're updating out of order, resort.
      (when (< (id message) (id (aref messages (1- (length messages)))))
        (setf (messages conv) (sort messages #'< :key #'id)))))
  (when (window 'main) (update-conversation conv (window 'main))))

(defmethod update-conversation ((conv conversation) (messages list))
  (for:for ((message in messages))
    (unless (find (id message) (messages conv) :key #'id)
      (vector-push-extend message (messages conv))))
  (setf (messages conv) (sort (messages conv) #'< :key #'id))
  (when (window 'main) (update-conversation conv (window 'main))))

(defclass direct-conversation (conversation)
  ((participant :initarg :participant :accessor participant))
  (:default-initargs
   :participant (error "PARTICIPANTS required.")))

(defmethod label ((conv direct-conversation))
  (name (participant conv)))

(defmethod reply ((conv direct-conversation) text)
  (let ((dm (chirp:direct-messages/new text :user-id (id (participant conv)))))
    (update-conversation conv (message dm))))

(defmethod participants ((conv direct-conversation))
  (list (participant conv)))

(defmethod avatar ((conv direct-conversation))
  (avatar (participant conv)))

(defmethod ensure-conversation ((user user))
  (or (conversation user)
      (make-instance 'direct-conversation :id (id user) :participant user)))

(defmethod ensure-conversation ((name string))
  (ensure-conversation (user name)))

(defclass group-conversation (conversation)
  ((participants :initarg :participants :accessor participants))
  (:default-initargs
   :participants (error "PARTICIPANTS required.")))

(defmethod label ((conv group-conversation))
  (format NIL "~{~a~^, ~}" (mapcar #'name (participants conv))))

(defmethod reply ((conv group-conversation) text)
  (error "How did you even manage to reach this method?"))

(defmethod avatar ((conv group-conversation))
  (make-instance 'avatar))

(defmethod ensure-conversation ((users list))
  (error "How did you even manage to reach this method?"))

(defun find-latest-id (&optional (convs (conversations T)))
  (for:for ((conv over convs)
            (messages = (messages conv))
            (id = (if (= 0 (length messages)) 0 (id (aref messages (1- (length messages))))))
            (max-id maximizing id))))

(defun fetch-paged (endpoint &key (per-request 200) since upto)
  (loop for max-id = upto then next-id
        for set = (funcall endpoint :count per-request :since-id since :max-id max-id)
        for next-id = (when set (chirp:id (car (last set))))
        while (and set (or (not max-id) (/= max-id next-id)))
        append set
        do (v:debug :chatter.conversations "Fetched page ~a ~a => ~a with ~a results." endpoint since max-id (length set))))

(defun fetch-direct-messages (&key (since 0) (per-request 200))
  (nconc (fetch-paged #'chirp:direct-messages :since since :per-request per-request)
         (fetch-paged #'chirp:direct-messages/sent :since since :per-request per-request)))

(defun split-direct-conversations (dms)
  (let ((map (make-hash-table :test 'eql)))
    (for:for ((dm in dms)
              (msg = (message dm)))
      (returning (alexandria:hash-table-values map))
      (push msg (gethash (participant msg) map)))))

(defun update-direct-conversations (&key (since (find-latest-id)))
  (v:info :chatter.conversations "Updating direct conversations~@[ since ~a~]." since)
  (let ((messages (fetch-direct-messages :since since)))
    (for:for ((msgs in (split-direct-conversations messages))
              (user = (participant (first msgs)))
              (conv = (ensure-conversation user))
              (conversations collecting conv))
      (update-conversation conv msgs))))
