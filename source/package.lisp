(defpackage :hu.dwim.bluez
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c)
  (:shadow
   #:listen
   #:hci-filter-clear
   #:hci-filter-set-ptype
   #:hci-filter-set-event
   )

  (:export
   ;; NOTE: stay away here from re-exporting stuff from hu.dwim.bluez.ffi, because that'll change the behavior of cl:shadow
   ;; and break the functions below.
   ;; http://paste.lisp.org/+3D97
   ;; 2015-10-19 #sbcl
   ;; (22:12:53) Xof: "If it is accessible as an internal symbol via use-package, it is first imported into package, then exported."
   ;; (22:12:59) Xof: (CLHS EXPORT)
   ;; (22:13:17) Xof: so since the symbol is exported from B, it is present (not inherited) in B, so shadow has no effect
   ;; (22:13:28) stassats: attila_lendvai: you need to use the :shadow option in defpackage
   ;; (22:14:36) attila_lendvai: ooh. managed to code in cl for a decade without having a clue... thanks guys!
   ;; (22:14:54) stassats: yeah, the order is important
   ;; (22:14:56) stassats: clhs defpackage
   ;; (22:14:56) specbot: http://www.lispworks.com/reference/HyperSpec/Body/m_defpkg.htm
   ;; (22:15:13) stassats: right before the Examples section it lists the order
   ))

(in-package :hu.dwim.bluez)

(defun import-all-owned-symbols (source-package target-package &key (overwrite nil))
  (setf source-package (find-package source-package))
  (setf target-package (find-package target-package))
  (let ((count 0))
    (do-symbols (symbol source-package)
      (let ((target-symbol-with-same-name (find-symbol (symbol-name symbol) target-package)))
        (when (and (eq (symbol-package symbol) source-package)
                   (or overwrite
                       (not target-symbol-with-same-name)))
          (when (and target-symbol-with-same-name
                     (not (eq symbol target-symbol-with-same-name))
                     overwrite)
            (unintern target-symbol-with-same-name target-package))
          (import symbol target-package)
          (incf count))))
    count))

(progn
  ;; NOTE: the order of these operations is important
  #+nil
  (wrap-functions-with-return-code-check
   :exceptions '(hu.dwim.bluez.ffi::read))
  (import-all-owned-symbols :hu.dwim.bluez.ffi :hu.dwim.bluez)
  (shadowing-import '(hu.dwim.bluez.ffi:close
                      hu.dwim.bluez.ffi:socket)
                    :hu.dwim.bluez))
