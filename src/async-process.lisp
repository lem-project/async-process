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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system (cmd)
    (ignore-errors (string-right-trim '(#\Newline) (uiop:run-program cmd :output :string)))))

(cffi:define-foreign-library async-process
  (:unix #.(format nil "libasyncprocess-~A-~A.so" (system "uname -m") (system "uname")))
  (:windows #.(format nil "libasyncprocess-~A.dll" (if (or #+x86-64 t)
						       "x86_64"
						       "x86"))))

(cffi:use-foreign-library async-process)

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer)
  (nonblock :boolean))

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

(defun create-process (command &key nonblock)
  (let* ((command (uiop:ensure-list command))
         (length (length command)))
    (cffi:with-foreign-object (argv :string (1+ length))
      (loop :for i :from 0
            :for c :in command
            :do (setf (cffi:mem-aref argv :string i) c))
      (setf (cffi:mem-aref argv :string length) (cffi:null-pointer))
      (%create-process argv nonblock))))
