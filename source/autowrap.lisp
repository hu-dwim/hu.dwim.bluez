(in-package :hu.dwim.bluez.ffi)

(autowrap:c-include '(hu.dwim.bluez autospec "bluez.h")
  :spec-path '(hu.dwim.bluez autospec)
  :exclude-arch ("i386-unknown-freebsd"
                 "x86_64-unknown-freebsd"
                 "i686-apple-darwin9"
                 "x86_64-apple-darwin9"
                 "i686-pc-windows-msvc"
                 "x86_64-pc-windows-msvc")
  :exclude-sources ("stdio.h"
                    "libio.h"
                    "string.h"
                    "stdlib.h"
                    "types.h"
                    "time.h"
                    "locale.h"
                    "sys/select.h")
  :exclude-definitions ("^_" "^va_list$")
  :no-accessors t)
