(defsystem :hu.dwim.bluez
  :description "Common Lisp FFI wrapper for libbluetooth, aka Bluez, which is a Linux Bluetooth stack."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:alexandria
               :cl-autowrap
               :cl-plus-c
               :trivial-garbage)
  :components ((:module "source"
                :serial t
                :components ((:file "autowrap")
                             (:file "package")
                             (:file "package-late")
                             (:file "bluez")))
               (:module "autospec"
                :components ((:static-file "bluez.h")))))
