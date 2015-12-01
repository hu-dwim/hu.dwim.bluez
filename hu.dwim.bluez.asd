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

(defsystem :hu.dwim.bluez/fancy
  :description "Fancier API extensions for hu.dwim.bluez in return for more dependencies."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :depends-on (:hu.dwim.bluez
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def)
  :components ((:module "source"
                :serial t
                :components ((:file "fancy")))))
