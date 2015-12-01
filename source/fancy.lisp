(in-package :hu.dwim.bluez)

(hu.dwim.def:def hu.dwim.def:package :hu.dwim.bluez/fancy
  (:use :cl
        :autowrap.minimal
        :plus-c
        :hu.dwim.bluez
        :hu.dwim.def
        :iterate
        :metabang-bind)
  (:local-nicknames
   (#:bluez :hu.dwim.bluez)
   (#:bluez.ffi :hu.dwim.bluez.ffi)))

(in-package :hu.dwim.bluez/fancy)

(def (with-macro* e :macro-only-arguments (devid-var-name fd-var-name)) with-open-bluetooth-socket
    (devid-var-name fd-var-name &key local-device remote-device)
  "LOCAL-DEVICE: mac address or a 'hci2' device name. REMOTE-DEVICE: a mac address."
  (check-type local-device (or null string))
  (check-type remote-device (or null string))
  (when (and local-device
             remote-device)
    (error "~S was called with both LOCAL-DEVICE and REMOTE-DEVICE, provide only one." 'with-open-bluetooth-socket))
  (bind ((device-id (cond
                      (remote-device
                       (c-with ((device-address bluez:bdaddr-t))
                         (c-fun/rc bluez:str2ba remote-device device-address)
                         (c-fun/rc bluez:hci-get-route device-address)))
                      (local-device
                       (c-fun/rc bluez:hci-devid local-device)))))
    (bind ((device-fd (c-fun/rc bluez:hci-open-dev device-id)))
     (unwind-protect
          (-with-macro/body- (device-fd fd-var-name) (device-id devid-var-name))
       (c-fun/rc bluez:hci-close-dev device-fd)))))

(def (class* eas) hci-connection ()
  ((socket)
   (hci-device-id)
   (hci-device-name)
   (mac-address)))

(def print-object hci-connection
  (format t "dev ~A (mac ~A), ~:[closed~;open (fd ~S)~]" (hci-device-id-of -self-) (mac-address-of -self-) (socket-of -self-) (socket-of -self-)))

(def special-variable *open-hci-connections*)

(def (with-macro* e) with-hci-connection-context ()
  (assert (not (boundp '*open-hci-connections*)) () "nesting ~S is not supported" -this-function/name-)
  (bind ((*open-hci-connections* ()))
    (unwind-protect
         (-with-macro/body-)
      (dolist (connection *open-hci-connections*)
        (handler-case
            (close-hci-connection connection)
          (serious-condition (e)
            (warn "Error while closing HCI-CONNECTION ~A: ~A" connection e)))))))

(def (function e) open-hci-connection (&key local-device remote-device (non-blocking? t))
  "LOCAL-DEVICE: mac address or a 'hci2' device name. REMOTE-DEVICE: a mac address."
  (check-type local-device (or null string))
  (check-type remote-device (or null string))
  (when (and local-device
             remote-device)
    (error "~S was called with both LOCAL-DEVICE and REMOTE-DEVICE, provide only one." -this-function/name-))
  (bind ((device-id (cond
                      (remote-device
                       (c-with ((device-address bluez:bdaddr-t))
                         (c-fun/rc bluez:str2ba remote-device device-address)
                         (c-fun/rc bluez:hci-get-route device-address)))
                      (local-device
                       (c-fun/rc bluez:hci-devid local-device))
                      (t
                       0)))
         ((:values device-name mac-address) (bluez:hci-device-name device-id))
         (socket (c-fun/rc bluez:hci-open-dev device-id))
         (connection (make-instance 'hci-connection
                                    :socket socket
                                    :hci-device-id device-id
                                    :hci-device-name device-name
                                    :mac-address mac-address)))
    (when non-blocking?
      (setf (fd-nonblocking-p socket) t))
    (push connection *open-hci-connections*)
    connection))

(def (function e) close-hci-connection (connection)
  (c-fun/rc bluez:hci-close-dev (socket-of connection))
  (setf (socket-of connection) nil)
  connection)
