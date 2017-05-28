(in-package :hu.dwim.bluez)

(hu.dwim.def:def hu.dwim.def:package :hu.dwim.bluez/fancy
  (:use :hu.dwim.common
        :hu.dwim.def
        :metabang-bind)
  (:import-from :hu.dwim.bluez
                c-fun
                c-fun/rc)
  (:local-nicknames
   (#:bluez :hu.dwim.bluez)
   (#:bluez.ffi :hu.dwim.bluez.ffi))
  ;; To be able to use C-c C-c in Slime: (asdf:load-systems :hu.dwim.def+swank :hu.dwim.bluez/fancy)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)
   (hu.dwim.syntax-sugar:enable-case-preserving-syntax :packages '(:hu.dwim.bluez.ffi))))
