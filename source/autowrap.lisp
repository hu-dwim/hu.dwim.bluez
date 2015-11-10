(defpackage :hu.dwim.bluez.ffi
  (:use))

(in-package :hu.dwim.bluez.ffi)

(autowrap:c-include '(hu.dwim.bluez autospec "bluez.h")
  :spec-path '(hu.dwim.bluez autospec)
  #+nil
  (:symbol-regex (("(?i)^(hci)_"
                   ()
                   "\\1/")))
  :sysincludes '(
                 ;; if this is not here, then memset doesn't show up in the spec (maybe because stddef.h is reported to be missing without it?)
                 "/usr/include/linux/")
  :exclude-arch ("i386-unknown-freebsd"
                 "x86_64-unknown-freebsd"
                 "i686-apple-darwin9"
                 "x86_64-apple-darwin9"
                 "i686-pc-windows-msvc"
                 "x86_64-pc-windows-msvc")
  :exclude-sources ("stdio.h"
                    "fcntl.h"
                    "unistd.h"
                    "libio.h"
                    "string.h"
                    "stdlib.h"
                    "types.h"
                    "time.h"
                    "locale.h"
                    "socket.h"
                    "sys/ioctl.h"
                    "sys/select.h")
  :include-definitions ("memset"
                        "size_t"
                        "ssize_t"
                        "socklen_t"
                        "getsockopt"
                        "setsockopt"
                        "read"
                        "close"
                        "ioctl"
                        "fcntl"
                        "strerror"
                        "F_SETFL"
                        "O_NONBLOCK"
                        ;; socket stuff
                        "socket"
                        "AF_BLUETOOTH"
                        "SOCK_RAW"
                        "BTPROTO_HCI"
                        )
  :exclude-definitions ("^_"
                        "^va_list$")
  :no-accessors t)
