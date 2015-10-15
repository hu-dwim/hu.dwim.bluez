(defpackage :hu.dwim.bluez.ffi
  (:use)
  (:export #:listen))

(defpackage :hu.dwim.bluez
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:hu.dwim.bluez.ffi)
  (:shadowing-import-from #:hu.dwim.bluez.ffi
                          #:listen)
  (:export

   ;; General
   #:errno #:strerror #:version

   ))

(in-package :hu.dwim.bluez)

(defun wrap-functions-with-return-value-check ()
  (loop
    :with bluez-package = (find-package :hu.dwim.bluez)
    :with bluez-ffi-package = (find-package :hu.dwim.bluez.ffi)
    :for fn-name :being :the :hash-keys :of autowrap::*foreign-functions* :using (hash-value fn-def)
    :when (eq (symbol-package fn-name) bluez-ffi-package)
    :do
    (let* ((fn-name/new (progn
                          (shadow fn-name bluez-package)
                          (intern (string fn-name) bluez-package)))
           (args (loop
                   :for arg :in (autowrap:foreign-record-fields fn-def)
                   :collect (autowrap:foreign-type-name arg)))
           (arg-tmps (loop
                       :for arg :in args
                       :collect (gensym)))
           (quoted-arg-tmps (mapcar (curry #'list 'quote) arg-tmps))
           (result-tmp (gensym (string '#:result))))
      (eval `(defmacro ,fn-name/new ,args
               `(let* (,,@(loop
                            :for arg :in args
                            :for arg-tmp :in arg-tmps
                            :collect `(list ',arg-tmp ,arg))
                       ,'(,result-tmp (,fn-name ,@arg-tmps)))
                  (when (minusp ,',result-tmp)
                    (error "FFI call failed. Name: ~S, args: ~S, return-code: ~S." ',',fn-name (list ,,@quoted-arg-tmps) ,',result-tmp))
                  ,',result-tmp))))))

(wrap-functions-with-return-value-check)
