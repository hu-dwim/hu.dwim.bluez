;;; This file is loaded before the generated FFI.

(in-package :hu.dwim.bluez)

(define-condition bluez-error (error)
  ())

(define-condition hci-error (bluez-error)
  ())

(define-condition hci-error/negative-return-code (simple-error hci-error)
  ((error-code :initform (error "Must specify ERROR-CODE.")
               :accessor error-code-of
               :initarg :error-code)))

(cffi:define-foreign-type hci-return-code (cffi::foreign-type-alias)
  ()
  (:default-initargs :actual-type (cffi::parse-type :int))
  (:simple-parser hci-return-code))

(defmethod cffi:expand-from-foreign (value (type hci-return-code))
  ;; NOTE: strictly speaking it should be (cffi:convert-from-foreign ,value :int), but not in this case.
  `(let ((return-code ,value))
     (if (< return-code 0)
         (error 'hci-error/negative-return-code
                :error-code return-code
                :format-control "Bluez HCI call failed: ~S ~S"
                :format-arguments (list return-code (strerror)))
         return-code)))

#+nil ;; we're fine with the default for now
(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (check-type name string)
  name)

(defun ffi-type-transformer (type context &rest args &key &allow-other-keys)
  (let ((type (apply 'cffi/c2ffi:default-ffi-type-transformer type context args)))
    (cond
      ((and (eq type :int)
            (consp context)
            (eq (first context) :function)
            (eq (third context) :return-type)
            (starts-with-subseq "hci_" (second context))
            (not (member (second context)
                         ;; a blacklist of hci function names (as a string)
                         ;; that should automatically signal error on failure.
                         '()
                         :test 'equal)))
       ;; this is a cffi type that automatically signals
       ;; an error if the return code is negative.
       'hci-return-code)
      #+nil
      ((equal context '(:struct "hci_dev_info" "name"))
       (assert (equal type '(:array :char 8)))
       ;; err, no, this dereferences a pointer
       :string)
      (t
       type))))
