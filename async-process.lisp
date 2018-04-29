(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process))
(in-package :async-process)

(pushnew (asdf:system-relative-pathname :async-process "./static/")
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library async-process
  (:unix "libasync-process.so"))

(cffi:use-foreign-library async-process)

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer))

(cffi:defcfun "delete_process" :void
  (process :pointer))

(cffi:defcfun "process_pid" :int
  (process :pointer))

(cffi:defcfun "process_send_input" :void
  (process :pointer)
  (string :string))

(cffi:defcfun "process_receive_output" :string
  (process :pointer))

(cffi:defcfun "process_alive_p" :boolean
  (process :pointer))

(defun create-process (command)
  (let* ((command (uiop:ensure-list command))
         (length (length command)))
    (cffi:with-foreign-object (argv :string (1+ length))
      (loop :for i :from 0
            :for c :in command
            :do (setf (cffi:mem-aref argv :string i) c))
      (setf (cffi:mem-aref argv :string length) (cffi:null-pointer))
      (%create-process argv))))
