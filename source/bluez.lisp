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

(defun %c-fun/rc/check-error (rc fn-name whole-form)
  (when (minusp rc)
    (error "FFI call failed. Name: ~S, return-code: ~S, errno: ~S, strerror: ~S, expression: ~S."
           fn-name rc autowrap:errno (hu.dwim.bluez.ffi:strerror autowrap:errno) whole-form)))

(defmacro c-fun/rc (&whole whole fn-name &rest args)
  (with-unique-names (rc)
    `(let ((,rc (c-fun ,fn-name ,@args)))
       (%c-fun/rc/check-error ,rc ',fn-name ',whole)
       ,rc)))

(defun copy-sap-to-byte-vector (pointer size)
  (loop
    :with result = (make-array size :element-type '(unsigned-byte 8))
    :for index :from 0 :below size
    :do (setf (aref result index)
              (cffi:mem-ref pointer :unsigned-char index))
    :finally (return result)))

(defun (setf fd-nonblocking-p) (enabled fd)
  (check-type fd fd)
  (let* ((current-flags (c-fun/rc fcntl fd +f-getfl+))
         (new-flags (if enabled
                        (logior current-flags +o-nonblock+)
                        (logand current-flags (lognot +o-nonblock+)))))
    (unless (eql current-flags new-flags)
      (format t "Setting fd ~S flags to ~S~%" fd new-flags)
      (c-fun/rc fcntl fd +f-setfl+ :int new-flags)))
  (values))

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

(defun hci-filter/initialize-for-le-scanning (hci-filter)
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
    (values (device-info :name string)
            (bdaddr->string (device-info :bdaddr &)))))

(defun hci/is-device-le-capable? (device-id)
  (check-type device-id hci-device-id)
  (c-with ((device-info hci-dev-info))
    (c-fun/rc hci-devinfo device-id device-info)
    ;; from: http://code.metager.de/source/xref/linux/bluetooth/bluez-hcidump/lib/hci.c#lmp_features_map
    (not (zerop (logand (cffi:mem-ref (device-info :features &) :uchar 4)
                        +lmp-le+)))))

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

(defun bdaddr->string (bdaddr)
  ;; this won't work because of the way c typedef is first an anonymous struct type and then an alias to that struct...
  ;;(check-type bdaddr bdaddr-t)
  ;; and it's valid to call it with e.g. (device-info :bdaddr &), so we cannot assert on the type
  ;;(check-type bdaddr autowrap:wrapper)
  (assert (not (cffi:null-pointer-p (ptr bdaddr))))
  (c-with ((address :char :count 32))
    (ba2str bdaddr (address &))
    (cffi:foreign-string-to-lisp (address &))))

(macrolet ((x (name value &optional doc)
             `(progn
                (defconstant ,name ,value ,doc)
                (export ',name))))
  (x +eir-flags+            #x01)
  (x +eir-uuid16-some+      #x02 "16-bit UUID, more available")
  (x +eir-uuid16-all+       #x03 "16-bit UUID, all listed")
  (x +eir-uuid32-some+      #x04 "32-bit UUID, more available")
  (x +eir-uuid32-all+       #x05  "32-bit UUID, all listed")
  (x +eir-uuid128-some+     #x06  "128-bit UUID, more available")
  (x +eir-uuid128-all+      #x07  "128-bit UUID, all listed")
  (x +eir-name-short+       #x08  "shortened local name")
  (x +eir-name-complete+    #x09  "complete local name")
  (x +eir-tx-power+         #x0A  "transmit power level")
  (x +eir-device-id+        #x10  "device ID"))

(defun parse-extended-inquiry-response (eir eir-length)
  (check-type eir cffi:foreign-pointer)
  (check-type eir-length alexandria:array-index)
  (loop
    :with offset = 0
    :with buffer = eir

    :while (< offset eir-length)
    :for field-length = (cffi:mem-ref buffer :unsigned-char offset)
    :until (zerop field-length)
    :while (<= (+ offset field-length) eir-length)
    :for field-type = (cffi:mem-ref buffer :unsigned-char 1)
    :appending (prog1
                   (flet ((field-as-string ()
                            (let ((name-length (1- field-length)))
                              (cffi:foreign-string-to-lisp buffer :offset 2 :count name-length :encoding :ascii))))
                     (case field-type
                       (#.+eir-name-short+
                        (list '+eir-name-short+ (field-as-string)))
                       (#.+eir-name-complete+
                        (list '+eir-name-complete+ (field-as-string)))
                       (t
                        #+nil
                        (format t "Unknown field while parsing: ~D~%" field-type))))
                 (incf offset (1+ field-length))
                 (cffi:incf-pointer buffer (1+ field-length)))))
