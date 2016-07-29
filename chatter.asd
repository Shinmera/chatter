#|
 This file is a part of chatter
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(asdf:defsystem chatter
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A twitter based chat client."
  :homepage "https://github.com/Shinmera/chatter"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "windows")
               (:file "resource")
               (:file "network")
               (:file "avatar")
               (:file "server")
               (:file "conversations")
               (:file "stream")
               (:file "login")
               (:file "chat")
               (:file "conversation-list")
               (:file "main"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore
               :qtgui
               :qtnetwork
               :chirp-dexador
               :bordeaux-threads
               :ubiquitous
               :hunchentoot
               :find-port
               :cl-ppcre
               :verbose
               :simple-tasks
               :alexandria
               :dissect
               :dexador)
  :build-operation "qt-program-op"
  :build-pathname "chatter"
  :entry-point "chatter:start")
