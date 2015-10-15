(in-package :hu.dwim.bluez)

(cffi:define-foreign-library libbluetooth
  (t (:default "libbluetooth")))

(cffi:use-foreign-library libbluetooth)

(defun device-name (device-id)
  (check-type device-id (integer 0))
  (c-with ((device-info (:struct (hci-dev-info))))
    (hci-devinfo device-id device-info)
    (device-info :name string)))

(defun test ()
  (let (
        (mac "00:1A:7D:DA:71:13")
        ;;(mac "C0:CB:38:AD:A2:61")
        )
    (c-with ((device-address bdaddr-t))
      (str2ba mac device-address)
      (let* (
             (device-id (hci-devid mac))
             ;;(device-id (hci-get-route device-address))
             (socket (hci-open-dev device-id)))
        (unwind-protect
             (format t "Socket is: ~S, mac is: ~S, device name is: ~S~%" socket mac (device-name device-id))
          (hci-close-dev socket))))))


#|

#+nil
  (with-alloc (device-info '(:struct (hci-dev-info)))
    (print (hci-devinfo device-id device-info))
    (format t "Name is: ~S~%" (c-ref device-info '(:struct (hci-dev-info)) :name))
    )


(defmacro check-null (form)
  (once-only (form)
    `(if (wrapper-null-p ,form)
         (error "NULL returned from ~A" ',form)
         ,form)))

(defmacro check-nil (form)
  (once-only (form)
    `(if (null ,form)
         (error "NIL returned from ~A" ',form)
         ,form)))

(defmacro check-rc (form)
  (once-only (form)
    `(if (< ,form 0)
         (let* ((errno (zmq-errno))
                (string (zmq-strerror errno)))
           (error "ZMQ error (~A): ~A" errno string))
         ,form)))

(defun errno () (zmq-errno))
(defun strerror (errno) (values (zmq-strerror errno)))

(defun version ()
  (c-with ((major :int)
           (minor :int)
           (patch :int))
    (zmq-version (major &) (minor &) (patch &))
    (values major minor patch)))

(autowrap:define-bitmask-from-constants (zmq-sendflags)
  +zmq-dontwait+
  +zmq-sndmore+)

(autowrap:define-bitmask-from-constants (zmq-recvflags "^ZMQ-")
  +zmq-dontwait+)

(declaim (inline slow-copy-to-c slow-copy-to-lisp))
(defun slow-copy-to-c (lisp-data c-data)
  (c-with ((c-data :char :ptr c-data))
    (loop for i from 0 below (length lisp-data)
          do (setf (c-data i) (aref lisp-data i)))
    c-data))

(defun slow-copy-to-lisp (c-data lisp-data)
  (c-with ((c-data :unsigned-char :ptr c-data))
    (loop for i from 0 below (length lisp-data)
          do (setf (aref lisp-data i) (c-data i))))
  lisp-data)
|#
