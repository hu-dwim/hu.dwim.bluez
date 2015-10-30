(in-package :hu.dwim.bluez)

(export
 (let ((*package* (find-package :hu.dwim.bluez)))
   '(
     ;;   #:errno
     ;;   #:strerror
     ;;   #:version

     c-fun/rc

     bdaddr-t
     hci-device-id
     hci-device-name
     str2ba
     hci-devid
     hci-open-dev
     hci-close-dev
     hci-le-set-scan-parameters
     hci-le-set-scan-enable
     +hci-max-event-size+

     hci/reset-device
     hci-filter/initialize-for-scanning

     hci-dev-info
     hci-filter
     hci-filter-clear
     hci-filter-set-ptype
     hci-filter-set-event

     uint8-t

     getsockopt
     setsockopt
     socklen-t
     +sol-hci+
     +hci-filter+
     +hci-event-pkt+
     +evt-le-meta-event+

     fcntl
     +f-getfl+
     +f-setfl+
     +o-nonblock+
     +ewouldblock+
     ))
 :hu.dwim.bluez)

;; define some convenience type aliases
(autowrap:define-foreign-alias 'hci-dev-info '(:struct (hci-dev-info)))
(autowrap:define-foreign-alias 'hci-filter '(:struct (hci-filter)))
