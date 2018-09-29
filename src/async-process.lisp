(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process))
(in-package :async-process)

(pushnew (asdf:system-relative-pathname :async-process "../static/")
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

(defclass process ()
  ((process :reader process-process :initarg :process)
   (encode :accessor process-encode :initarg :encode)))

(cffi:defcfun ("create_process" %create-process) :pointer
  (command :pointer)
  (nonblock :boolean))

(cffi:defcfun ("delete_process" %delete-process) :void
  (process :pointer))

(cffi:defcfun ("process_pid" %process-pid) :int
  (process :pointer))

(cffi:defcfun ("process_send_input" %process-send-input) :void
  (process :pointer)
  (string :string))

(cffi:defcfun ("process_receive_output" %process-receive-output) :string
  (process :pointer))

(cffi:defcfun ("process_alive_p" %process-alive-p) :boolean
  (process :pointer))

(defun create-process (command &key nonblock (encode cffi:*default-foreign-encoding*))
  (let* ((command (uiop:ensure-list command))
         (length (length command)))
    (cffi:with-foreign-object (argv :string (1+ length))
      (loop :for i :from 0
            :for c :in command
            :do (setf (cffi:mem-aref argv :string i) c))
      (setf (cffi:mem-aref argv :string length) (cffi:null-pointer))
      (make-instance 'process
		     :process (%create-process argv nonblock)
		     :encode  encode))))

(defun delete-process (process)
  (%delete-process (process-process process)))

(defun process-pid (process)
  (%process-pid (process-process process)))

(defun process-send-input (process string)
  (let ((cffi:*default-foreign-encoding* (process-encode process)))
    (%process-send-input (process-process process) string)))

(defun process-receive-output (process)
  (let ((cffi:*default-foreign-encoding* (process-encode process)))
    (%process-receive-output (process-process process))))

(defun process-alive-p (process)
  (%process-alive-p (process-process process)))
