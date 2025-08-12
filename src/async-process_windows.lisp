(defpackage :async-process
  (:use :cl)
  (:export
   :delete-process
   :process-send-input
   :process-receive-output
   :process-alive-p
   :create-process))
(in-package :async-process)

;; Windows API definitions via CFFI
(cffi:define-foreign-library kernel32
  (:windows "kernel32.dll"))

(cffi:use-foreign-library kernel32)

;; Constants
(defconstant +invalid-handle-value+ #xFFFFFFFF)
(defconstant +create-no-window+ #x08000000)
(defconstant +startf-usestdhandles+ #x00000100)
(defconstant +startf-useshowwindow+ #x00000001)
(defconstant +sw-hide+ 0)
(defconstant +duplicate-same-access+ #x00000002)
(defconstant +still-active+ 259)

;; Structures
(cffi:defcstruct security-attributes
  (length :uint32)
  (security-descriptor :pointer)
  (inherit-handle :boolean))

(cffi:defcstruct startup-info
  (cb :uint32)
  (reserved :pointer)
  (desktop :pointer)
  (title :pointer)
  (x :uint32)
  (y :uint32)
  (x-size :uint32)
  (y-size :uint32)
  (x-count-chars :uint32)
  (y-count-chars :uint32)
  (fill-attribute :uint32)
  (flags :uint32)
  (show-window :uint16)
  (cb-reserved2 :uint16)
  (reserved2 :pointer)
  (std-input :pointer)
  (std-output :pointer)
  (std-error :pointer))

(cffi:defcstruct process-information
  (process :pointer)
  (thread :pointer)
  (process-id :uint32)
  (thread-id :uint32))

;; Windows API functions
(cffi:defcfun ("CreatePipe" create-pipe) :boolean
  (read-pipe (:pointer :pointer))
  (write-pipe (:pointer :pointer))
  (pipe-attributes (:pointer (:struct security-attributes)))
  (size :uint32))

(cffi:defcfun ("CreateProcessW" create-process-w) :boolean
  (application-name :pointer)
  (command-line :pointer)
  (process-attributes :pointer)
  (thread-attributes :pointer)
  (inherit-handles :boolean)
  (creation-flags :uint32)
  (environment :pointer)
  (current-directory :pointer)
  (startup-info (:pointer (:struct startup-info)))
  (process-information (:pointer (:struct process-information))))

(cffi:defcfun ("DuplicateHandle" duplicate-handle) :boolean
  (source-process :pointer)
  (source-handle :pointer)
  (target-process :pointer)
  (target-handle (:pointer :pointer))
  (desired-access :uint32)
  (inherit-handle :boolean)
  (options :uint32))

(cffi:defcfun ("GetCurrentProcess" get-current-process) :pointer)

(cffi:defcfun ("CloseHandle" close-handle) :boolean
  (object :pointer))

(cffi:defcfun ("WriteFile" write-file) :boolean
  (file :pointer)
  (buffer :string)
  (number-of-bytes-to-write :uint32)
  (number-of-bytes-written (:pointer :uint32))
  (overlapped :pointer))

(cffi:defcfun ("ReadFile" read-file) :boolean
  (file :pointer)
  (buffer :pointer)
  (number-of-bytes-to-read :uint32)
  (number-of-bytes-read (:pointer :uint32))
  (overlapped :pointer))

(cffi:defcfun ("PeekNamedPipe" peek-named-pipe) :boolean
  (pipe :pointer)
  (buffer :pointer)
  (buffer-size :uint32)
  (bytes-read (:pointer :uint32))
  (total-bytes-avail (:pointer :uint32))
  (bytes-left-this-message (:pointer :uint32)))

(cffi:defcfun ("GetExitCodeProcess" get-exit-code-process) :boolean
  (process :pointer)
  (exit-code (:pointer :uint32)))

(cffi:defcfun ("TerminateProcess" terminate-process) :boolean
  (process :pointer)
  (exit-code :uint32))

;; Process class
(defclass process ()
  ((process-info :accessor process-process-info :initarg :process-info)
   (input-handle :accessor process-input-handle :initarg :input-handle)
   (output-handle :accessor process-output-handle :initarg :output-handle)
   (nonblock :accessor process-nonblock :initarg :nonblock)
   (encode :accessor process-encode :initarg :encode)))

(defun create-process (command &key nonblock (encode cffi:*default-foreign-encoding*) directory)
  "Create a new process with the given command"
  (when (and directory (not (uiop:directory-exists-p directory)))
    (error "Directory ~S does not exist" directory))
  
  (let ((command-string (if (listp command)
                           (format nil "~{~A~^ ~}" command)
                           command)))
    
    (cffi:with-foreign-objects ((sa '(:struct security-attributes))
                                (output-read-tmp :pointer)
                                (output-write :pointer)
                                (input-read :pointer)
                                (input-write-tmp :pointer)
                                (output-read :pointer)
                                (input-write :pointer)
                                (si '(:struct startup-info))
                                (pi- '(:struct process-information)))
      
      ;; Initialize security attributes
      (setf (cffi:foreign-slot-value sa '(:struct security-attributes) 'length)
            (cffi:foreign-type-size '(:struct security-attributes)))
      (setf (cffi:foreign-slot-value sa '(:struct security-attributes) 'security-descriptor)
            (cffi:null-pointer))
      (setf (cffi:foreign-slot-value sa '(:struct security-attributes) 'inherit-handle)
            t)
      
      ;; Create pipes
      (unless (create-pipe output-read-tmp output-write sa 0)
        (error "Failed to create output pipe"))
      
      (unless (create-pipe input-read input-write-tmp sa 0)
        (error "Failed to create input pipe"))
      
      (let ((curr-process (get-current-process)))
        ;; Duplicate handles
        (unless (duplicate-handle curr-process
                                 (cffi:mem-ref output-read-tmp :pointer)
                                 curr-process
                                 output-read
                                 0 nil +duplicate-same-access+)
          (error "Failed to duplicate output read handle"))
        
        (unless (duplicate-handle curr-process
                                 (cffi:mem-ref input-write-tmp :pointer)
                                 curr-process
                                 input-write
                                 0 nil +duplicate-same-access+)
          (error "Failed to duplicate input write handle")))
      
      ;; Close temporary handles
      (close-handle (cffi:mem-ref output-read-tmp :pointer))
      (close-handle (cffi:mem-ref input-write-tmp :pointer))
      
      ;; Initialize startup info
      (cffi:foreign-funcall "memset" :pointer si :int 0 :uint32 
                           (cffi:foreign-type-size '(:struct startup-info)) :void)
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'cb)
            (cffi:foreign-type-size '(:struct startup-info)))
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'flags)
            (logior +startf-usestdhandles+ +startf-useshowwindow+))
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'show-window)
            +sw-hide+)
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'std-input)
            (cffi:mem-ref input-read :pointer))
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'std-output)
            (cffi:mem-ref output-write :pointer))
      (setf (cffi:foreign-slot-value si '(:struct startup-info) 'std-error)
            (cffi:mem-ref output-write :pointer))
      
      ;; Create process
      (cffi:with-foreign-strings ((cmd-wide command-string :encoding :utf-16le)
                                  (dir-wide (if directory (namestring directory) "")))
        (unless (create-process-w (cffi:null-pointer)
                                 cmd-wide
                                 (cffi:null-pointer)
                                 (cffi:null-pointer)
                                 t
                                 +create-no-window+
                                 (cffi:null-pointer)
                                 (if directory dir-wide (cffi:null-pointer))
                                 si
                                 pi-)
          (error "Failed to create process: ~A" command-string)))
      
      ;; Close handles we don't need
      (close-handle (cffi:mem-ref output-write :pointer))
      (close-handle (cffi:mem-ref input-read :pointer))
      
      ;; Create and return process object
      (make-instance 'process
                     :process-info pi-
                     :input-handle (cffi:mem-ref input-write :pointer)
                     :output-handle (cffi:mem-ref output-read :pointer)
                     :nonblock nonblock
                     :encode encode))))

(defun delete-process (process)
  "Terminate and clean up the process"
  (let ((pi- (process-process-info process)))
    (terminate-process (cffi:foreign-slot-value pi- '(:struct process-information) 'process) 2)
    (close-handle (process-input-handle process))
    (close-handle (process-output-handle process))
    (close-handle (cffi:foreign-slot-value pi- '(:struct process-information) 'thread))
    (close-handle (cffi:foreign-slot-value pi- '(:struct process-information) 'process))))

(defun process-pid (process)
  "Get the process ID"
  (cffi:foreign-slot-value (process-process-info process) '(:struct process-information) 'process-id))

(defun process-send-input (process string)
  "Send input to the process"
  (cffi:with-foreign-object (bytes-written :uint32)
    (let ((cffi:*default-foreign-encoding* (process-encode process)))
      (write-file (process-input-handle process)
                  string
                  (length string)
                  bytes-written
                  (cffi:null-pointer)))))

(defun process-receive-output (process)
  "Receive output from the process"
  (let ((buffer-size 4096))
    (cffi:with-foreign-objects ((buffer :char buffer-size)
                                (bytes-read :uint32)
                                (bytes-avail :uint32))
      
      ;; Check if data is available (for non-blocking mode)
      (when (process-nonblock process)
        (unless (peek-named-pipe (process-output-handle process)
                                (cffi:null-pointer) 0
                                (cffi:null-pointer)
                                bytes-avail
                                (cffi:null-pointer))
          (return-from process-receive-output nil))
        (when (zerop (cffi:mem-ref bytes-avail :uint32))
          (return-from process-receive-output nil)))
      
      ;; Read data
      (when (read-file (process-output-handle process)
                       buffer
                       (1- buffer-size)
                       bytes-read
                       (cffi:null-pointer))
        (let ((num-bytes (cffi:mem-ref bytes-read :uint32)))
          (when (> num-bytes 0)
            (setf (cffi:mem-aref buffer :char num-bytes) 0)
            (let ((cffi:*default-foreign-encoding* (process-encode process)))
              (cffi:foreign-string-to-lisp buffer :count num-bytes))))))))

(defun process-alive-p (process)
  "Check if the process is still running"
  (cffi:with-foreign-object (exit-code :uint32)
    (let ((pi- (process-process-info process)))
      (when (get-exit-code-process (cffi:foreign-slot-value pi- '(:struct process-information) 'process)
                                   exit-code)
        (= (cffi:mem-ref exit-code :uint32) +still-active+)))))