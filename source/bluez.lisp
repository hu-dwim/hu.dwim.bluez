(in-package :hu.dwim.bluez)

;; TODO HCI is little-endian. these should be swapping bytes when run on big-endian hosts.
(declaim (inline htob/16 htob/32 htob/64 htob/128))

(defun htob/16 (value)
  (check-type value (or (signed-byte 16)
                        (unsigned-byte 16)))
  ;; (ldb (byte 16 0) -42)
  ;; (logand -42 #xffff)
  value)

(defun htob/32 (value)
  (check-type value (or (signed-byte 32)
                        (unsigned-byte 32)))
  value)

(defun htob/64 (value)
  (check-type value (or (signed-byte 64)
                        (unsigned-byte 64)))
  value)

(defun htob/128 (value)
  (check-type value (or (signed-byte 128)
                        (unsigned-byte 128)))
  value)

;;;;;;
;;; types

(deftype hci/device-id ()
  ;; 1023 is an arbitrary limit on the max nubmer of devices
  '(integer 0 1023))

(deftype fd ()
  '(integer 1))

;;;;;;
;;; utils

(defun %c-fun/rc/check-error (rc fn-name whole-form)
  (when (minusp rc)
    (error "FFI call failed. Name: ~S, return-code: ~S, errno: ~S, strerror: ~S, expression: ~S."
           fn-name rc cffi:*errno* (strerror cffi:*errno*) whole-form)))

#+nil ;; TODO add something like this, but it needs to get debugged
(defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'c-fun/rc)) argument-forms)
  (if argument-forms
      (swank::compute-enriched-decoded-arglist 'swank::zork argument-forms)
      (call-next-method)))

#+nil
(defmethod swank:arglist-dispatch ((operator (eql 'c-fun/rc)) argument-forms)
  (block nil
    (let ((function-name (first argument-forms)))
      (when (and function-name
                 (symbolp function-name))
        (let ((arglist (swank:arglist-dispatch function-name (rest argument-forms))))
          )))
    (call-next-method)))

(defmacro c-fun/rc (&whole whole fn-name &rest args)
  (with-unique-names (rc)
    `(let ((,rc (,fn-name ,@args)))
       (%c-fun/rc/check-error ,rc ',fn-name ',whole)
       ,rc)))

;; TODO delme?
(defmacro c-fun (fn-name &rest args)
  `(,fn-name ,@args))

;; TODO handle this: (c-ref object type slot-name count) where count should count in the referenced type size
(defmacro c-ref (object type slot)
  `(foreign-slot-value ,object ',type ',slot))

(defun copy-sap-to-byte-vector (pointer size)
  (loop
    :with result = (make-array size :element-type '(unsigned-byte 8))
    :for index :from 0 :below size
    :do (setf (aref result index)
              (cffi:mem-ref pointer :unsigned-char index))
    :finally (return result)))

(defun (setf fd-nonblocking-p) (enabled fd)
  (check-type fd fd)
  (let* ((current-flags (c-fun/rc fcntl fd +f_getfl+))
         (new-flags (if enabled
                        (logior current-flags +o_nonblock+)
                        (logand current-flags (lognot +o_nonblock+)))))
    (unless (eql current-flags new-flags)
      (c-fun/rc fcntl fd +f_setfl+ :int new-flags)))
  (values))

;;;;;;
;;; API

(defun hci-filter/clear (filter)
  (check-type filter foreign-pointer)
  ;;(check-type filter hci_filter)
  ;; TODO add something to CFFI to make cffi:foreign-type-size happen at compile time?
  (c-fun memset filter 0 (load-time-value (cffi:foreign-type-size '(:struct hci_filter))))
  filter)

(defun hci-filter/set-ptype (type filter)
  ;;(check-type filter hci-filter)
  (check-type filter foreign-pointer)
  (check-type type (integer 0 31))
  ;; 	hci_set_bit((t == HCI_VENDOR_PKT) ? 0 : (t & HCI_FLT_TYPE_BITS), &f->type_mask);
  (let ((type-mask (c-ref filter (:struct hci_filter) type_mask)))
    (check-type type-mask (unsigned-byte 32))
    (setf (ldb (byte 1 (if (= type +hci_vendor_pkt+)
                           0
                           (logand type +hci_flt_type_bits+)))
               type-mask)
          1)
    (setf (c-ref filter (:struct hci_filter) type_mask) type-mask))
  filter)

(defun hci-filter/set-event (event filter)
  ;;(check-type hci-filter hci-filter)
  (check-type event (integer 0 63))
  ;; 	hci_set_bit((e & HCI_FLT_EVENT_BITS), &f->event_mask);
  (let* ((event (logand event +hci_flt_event_bits+)))
    (multiple-value-bind
          (word-offset bit-offset)
        (floor event 32)
      #+nil
      (setf (ldb (byte 1 (logand event +hci-flt-event-bits+))
                 event-mask)
            1)
      ;; TODO use c-ref if possible
      (setf (ldb (byte 1 bit-offset) (mem-aref (foreign-slot-pointer filter '(:struct hci_filter) 'event_mask)
                                               'uint32_t word-offset))
            1)
      #+nil
      (let ((value (c-ref hci-filter hci-filter :event-mask word-offset)))
        (setf (ldb (byte 1 bit-offset) value) 1)
        (setf (c-ref hci-filter hci-filter :event-mask word-offset) value))))
  filter)

(defun hci-filter/initialize-for-le-scanning (filter)
  ;;(check-type hci-filter hci-filter)
  (hci-filter/clear filter)
  (hci-filter/set-ptype +hci_event_pkt+ filter)
  (hci-filter/set-event +evt_le_meta_event+ filter)
  (hci-filter/set-event +evt_le_advertising_report+ filter)
  filter)

(defun hci/device-name (device-id)
  (check-type device-id hci/device-id)
  (with-foreign-object (device-info '(:struct hci_dev_info))
    (c-fun/rc hci_devinfo device-id device-info)
    ;; which one is more readable? maybe use something like (defmacro cref (&rest x) x) for readability?
    ;;(c-ref device-info hci-dev-info :name string)
    (values (foreign-string-to-lisp (foreign-slot-pointer device-info '(:struct hci_dev_info) 'name))
            ;; TODO this seems to dereference a pointer that we don't want (c-ref device-info (:struct hci_dev_info) name)
            (bdaddr->string (foreign-slot-pointer device-info '(:struct hci_dev_info) 'bdaddr)))))

(defun hci/is-device-le-capable? (device-id)
  (check-type device-id hci/device-id)
  (with-foreign-object (device-info '(:struct hci_dev_info))
    (c-fun/rc hci_devinfo device-id device-info)
    ;; from: http://code.metager.de/source/xref/linux/bluetooth/bluez-hcidump/lib/hci.c#lmp_features_map
    (not (zerop (logand (cffi:mem-ref (foreign-slot-pointer device-info '(:struct hci_dev_info) 'features)
                                      :uchar 4)
                        +lmp_le+)))))

(defun hci/shutdown-device (socket device-id)
  (check-type device-id hci/device-id)
  (check-type socket fd)
  (c-fun/rc ioctl socket +hcidevdown+ :int device-id))

(defun hci/bringup-device (socket device-id)
  (check-type device-id hci/device-id)
  (check-type socket fd)
  (c-fun/rc ioctl socket +hcidevup+ :int device-id))

(defun hci/reset-device (device-id)
  (check-type device-id hci/device-id)
  (let ((socket (c-fun/rc socket +af_bluetooth+ +sock_raw+ +btproto_hci+)))
    (unwind-protect
         ;; NOTE: +hcidevreset+ is broken in some way. when using this to reset the device then
         ;; scanning won't yield any results afterwards. see also the source of hciconfig:
         ;; (c-fun/rc ioctl socket +hcidevreset+ :int device-id)
         (progn
           (hci/shutdown-device socket device-id)
           (hci/bringup-device socket device-id))
      (c-fun/rc close socket)))
  (values))

(defun bdaddr->string (bdaddr)
  (check-type bdaddr foreign-pointer)
  (assert (not (cffi:null-pointer-p bdaddr)))
  (with-foreign-object (address :char 32)
    (ba2str bdaddr address)
    (cffi:foreign-string-to-lisp address)))

(defun string->bdaddr (str bdaddr)
  (check-type str string)
  (check-type bdaddr foreign-pointer)
  (assert (not (cffi:null-pointer-p bdaddr)))
  (c-fun/rc str2ba str bdaddr))

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
                       (#.+eir-flags+
                        (list '+eir-flags+ (cffi:mem-ref buffer :char)))
                       (t
                        (warn "~S: Unknown field while parsing: ~D~%" 'parse-extended-inquiry-response field-type))))
                 (incf offset (1+ field-length))
                 (cffi:incf-pointer buffer (1+ field-length)))))
