(in-package :hu.dwim.bluez)

;; TODO FIXME this should be grovelled by autowrap, but doesn't work for now
;; NOTE: be careful, there's also +hci-dev-down+ & co with different values. congratulations for whoever did this...
(defconstant +hcidevreset+ #x400448cb)
(defconstant +hcidevdown+  #x400448ca)
(defconstant +hcidevup+    #x400448c9)

;;;;;;
;;; library loading

;; TODO do we need an asdf:register-image-restore-hook ?

(cffi:define-foreign-library libbluetooth
  (t (:default "libbluetooth")))

(cffi:use-foreign-library libbluetooth)

;;;;;;
;;; types

(deftype hci-device-id ()
  ;; 1023 is an arbitrary limit on the max nubmer of devices
  '(integer 0 1023))

(deftype fd ()
  '(integer 1))

;;;;;;
;;; utils

(defmacro c-fun/rc (&whole whole fn-name &rest args)
  (with-unique-names (rc)
    `(let ((,rc (c-fun ,fn-name ,@args)))
       (when (minusp ,rc)
         (error "FFI call failed. Name: ~S, return-code: ~S, errno: ~S, expression: ~S." ',fn-name ,rc autowrap:errno ',whole))
       ,rc)))

(defun copy-sap-to-byte-vector (pointer size)
  (loop
    :with result = (make-array size :element-type '(unsigned-byte 8))
    :for index :from 0 :below size
    :do (setf (aref result index)
              (cffi:mem-ref pointer :unsigned-char index))
    :finally (return result)))

;;;;;;
;;; API

(defun hci-filter-clear (hci-filter)
  (check-type hci-filter hci-filter)
  (c-fun memset hci-filter 0 (load-time-value (autowrap:sizeof 'hci-filter)))
  hci-filter)

(defun hci-filter-set-ptype (type hci-filter)
  (check-type hci-filter hci-filter)
  (check-type type (integer 0 31))
  ;; 	hci_set_bit((t == HCI_VENDOR_PKT) ? 0 : (t & HCI_FLT_TYPE_BITS), &f->type_mask);
  (let ((type-mask (c-ref hci-filter hci-filter :type-mask)))
    (check-type type-mask (unsigned-byte 32))
    (setf (ldb (byte 1 (if (= type +hci-vendor-pkt+)
                           0
                           (logand type +hci-flt-type-bits+)))
               type-mask)
          1)
    (setf (c-ref hci-filter hci-filter :type-mask) type-mask))
  hci-filter)

(defun hci-filter-set-event (event hci-filter)
  (check-type hci-filter hci-filter)
  (check-type event (integer 0 63))
  ;; 	hci_set_bit((e & HCI_FLT_EVENT_BITS), &f->event_mask);
  (let* ((event (logand event +hci-flt-event-bits+)))
    (multiple-value-bind
          (word-offset bit-offset)
        (floor event 32)
      #+nil
      (setf (ldb (byte 1 (logand event +hci-flt-event-bits+))
                 event-mask)
            1)
      (setf (ldb (byte 1 bit-offset) (c-ref hci-filter hci-filter :event-mask word-offset)) 1)
      #+nil
      (let ((value (c-ref hci-filter hci-filter :event-mask word-offset)))
        (setf (ldb (byte 1 bit-offset) value) 1)
        (setf (c-ref hci-filter hci-filter :event-mask word-offset) value))))
  hci-filter)

(defun hci-filter/initialize-for-scanning (hci-filter)
  (check-type hci-filter hci-filter)
  (hci-filter-clear hci-filter)
  (hci-filter-set-ptype +hci-event-pkt+ hci-filter)
  (hci-filter-set-event +evt-le-meta-event+ hci-filter)
  hci-filter)

(defun hci-device-name (device-id)
  (check-type device-id hci-device-id)
  (c-with ((device-info hci-dev-info))
    (c-fun/rc hci-devinfo device-id device-info)
    ;; which one is more readable? maybe use something like (defmacro cref (&rest x) x) for readability?
    ;;(c-ref device-info hci-dev-info :name string)
    (device-info :name string)))

(defun hci/shutdown-device (socket device-id)
  (check-type device-id hci-device-id)
  (check-type socket fd)
  (c-fun/rc ioctl socket +hcidevdown+ :int device-id))

(defun hci/bringup-device (socket device-id)
  (check-type device-id hci-device-id)
  (check-type socket fd)
  (c-fun/rc ioctl socket +hcidevup+ :int device-id))

(defun hci/reset-device (device-id)
  (check-type device-id hci-device-id)
  (let ((socket (c-fun/rc socket +af-bluetooth+ +sock-raw+ +btproto-hci+)))
    (unwind-protect
         ;; NOTE: this call below is broken in some way. when using this to reset the device then
         ;; scanning won't yield any results afterwards. see also the source of hciconfig:
         ;; (c-fun/rc ioctl socket +hcidevreset+ :int device-id)
         (progn
           (hci/shutdown-device socket device-id)
           (hci/bringup-device socket device-id))
      (c-fun/rc close socket)))
  (values))
