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
  ;; NOTE: the PROGN to marks that the order of these operations is important
  (import-all-owned-symbols :hu.dwim.bluez.ffi :hu.dwim.bluez)
  (shadowing-import '(hu.dwim.bluez.ffi::close
                      hu.dwim.bluez.ffi::socket)
                    :hu.dwim.bluez))
