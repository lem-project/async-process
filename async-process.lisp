(defpackage :async-process
  (:use :cl)
  (:export
   :process-send-input
   :process-receive-input
   :process-alive-p
   :create-process))
(in-package :async-process)

(pushnew (asdf:system-source-directory :async-process)
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library async-process
  (:unix "async-process.so"))

(cffi:use-foreign-library async-process)

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer))

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
