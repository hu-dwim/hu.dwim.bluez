(in-package :hu.dwim.bluez)

(export
 (let ((*package* (find-package :hu.dwim.bluez)))
   '(
     ;;   #:errno
     ;;   #:strerror
     ;;   #:version

     c-fun/rc

     hci/device-id
     hci/device-name
     hci/reset-device
     hci/is-device-le-capable?
     hci-filter/initialize-for-le-scanning
     hci-filter/clear
     hci-filter/set-ptype
     hci-filter/set-event
     bdaddr->string
     string->bdaddr
     parse-extended-inquiry-response
     fd-nonblocking-p

     ;; reexported ffi stuff
     libbluetooth
     bdaddr_t
     str2ba
     hci_devid
     hci_get_route
     hci_open_dev
     hci_close_dev
     hci_le_set_scan_parameters
     hci_le_set_scan_enable
     le_advertising_info

     hci_dev_info
     hci_filter

     uint8_t
     uint16_t
     uint32_t

     htob/16
     htob/32
     htob/64
     htob/128

     getsockopt
     setsockopt
     socklen_t
     +sol_hci+
     +hci_filter+
     +hci_event_pkt+
     +hci_event_hdr_size+
     +hci_max_event_size+

     +evt_le_meta_event+
     +evt_le_advertising_report+

     +le_public_address+

     fcntl
     fd_nonblocking_p
     +f_getfl+
     +f_setfl+
     +o_nonblock+
     +ewouldblock+
     +eintr+
     ))
 :hu.dwim.bluez)
