(defsystem :hu.dwim.bluez
  :description "Common Lisp FFI wrapper for libbluetooth, aka Bluez, which is a Linux Bluetooth stack."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:alexandria
               :cffi
               :cffi/c2ffi
               :cffi-libffi)
  :components ((:file "package-stage-1"
                :pathname "source/package-stage-1")
               (:file "ffi-prelude"
                :pathname "source/ffi-prelude"
                :depends-on ("package-stage-1"))
               (:module "source"
                :depends-on ("c2ffi-spec" "package-stage-1")
                :serial t
                :components ((:file "package-stage-2")
                             (:file "package-stage-3")
                             (:file "bluez")))
               (:module "c2ffi-spec"
                :depends-on ("ffi-prelude")
                :components ((:cffi/c2ffi-file "bluez.h"
                              :package #:hu.dwim.bluez.ffi
                              :rule-matcher :ends-with-subseq
                              ;; :ffi-name-transformer "hu.dwim.bluez::ffi-name-transformer"
                              :ffi-type-transformer "hu.dwim.bluez::ffi-type-transformer"
                              :foreign-library-name "hu.dwim.bluez.ffi::libbluetooth"
                              :foreign-library-spec ((t (:default "libbluetooth")))
                              ;; AFAIK BlueZ is Linux only.
                              :exclude-archs ("i386-unknown-freebsd"
                                              "x86_64-unknown-freebsd"
                                              "i686-apple-darwin9"
                                              "x86_64-apple-darwin9"
                                              "i686-pc-windows-msvc"
                                              "x86_64-pc-windows-msvc")
                              :include-sources ("bits/types.h"
                                                "sys/types.h"
                                                "stdint.h"
                                                "errno.h"
                                                "errno-base.h"
                                                "bluetooth/bluetooth.h"
                                                "bluetooth/hci.h"
                                                "bluetooth/hci_lib.h")
                              :exclude-sources :all
                              :include-definitions ("size_t"
                                                    "ssize_t"
                                                    "memset"
                                                    "socklen_t"
                                                    "getsockopt"
                                                    "setsockopt"
                                                    "read"
                                                    "close"
                                                    "ioctl"
                                                    "fcntl"
                                                    "F_SETFL"
                                                    "F_GETFL"
                                                    "O_NONBLOCK"
                                                    ;; socket stuff
                                                    "socket"
                                                    "AF_BLUETOOTH"
                                                    "SOCK_RAW"
                                                    "BTPROTO_HCI"
                                                    "HCIDEVRESET"
                                                    ;; NOTE: watch out, their meaning is wildly different! congratulations for whoever did this...
                                                    "HCIDEVDOWN"
                                                    "HCI_DEV_DOWN"
                                                    "HCIDEVUP"
                                                    "HCI_DEV_UP"
                                                    )
                              :exclude-definitions ())))))

(defsystem :hu.dwim.bluez/fancy
  :description "Fancier API extensions for hu.dwim.bluez in return for more dependencies."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.bluez
               :hu.dwim.def+hu.dwim.common
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :serial t
                :components ((:file "package-fancy")
                             (:file "fancy")))))
