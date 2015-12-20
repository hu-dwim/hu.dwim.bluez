(in-package :hu.dwim.bluez)

(export
 (let ((*package* (find-package :hu.dwim.bluez)))
   '(
     ;;   #:errno
     ;;   #:strerror
     ;;   #:version

     libbluetooth

     c-fun/rc

     bdaddr_t
     hci-device-id
     hci-device-name
     str2ba
     hci-devid
     hci-get-route
     hci-open-dev
     hci-close-dev
     hci-le-set-scan-parameters
     hci-le-set-scan-enable
     le-advertising-info

     hci/reset-device
     hci/is-device-le-capable?
     hci-filter/initialize-for-le-scanning
     bdaddr->string
     string->bdaddr
     parse-extended-inquiry-response

     hci-dev-info
     hci-filter
     hci-filter-clear
     hci-filter-set-ptype
     hci-filter-set-event

     uint8-t
     uint16-t
     uint32-t

     htob/16
     htob/32
     htob/64
     htob/128

     getsockopt
     setsockopt
     socklen-t
     +sol-hci+
     +hci-filter+
     +hci-event-pkt+
     +hci-event-hdr-size+
     +hci-max-event-size+

     +evt-le-meta-event+
     +evt-le-advertising-report+

     +le-public-address+

     fcntl
     fd-nonblocking-p
     +f-getfl+
     +f-setfl+
     +o-nonblock+
     +ewouldblock+
     +eintr+
     ))
 :hu.dwim.bluez)
