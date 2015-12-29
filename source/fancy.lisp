(in-package :hu.dwim.bluez/fancy)

;; this is a channel to the local bluetooth adapter that talks the Host Controller Interface protocol.
(def (class* eas) hci-socket ()
  ((fd)
   (hci-adapter-id)
   (hci-adapter-name)
   (mac-address)))

(def print-object hci-socket
  (format t "dev ~A (mac ~A), ~:[closed~;open (fd ~S)~]" (hci-adapter-id-of -self-) (mac-address-of -self-) (fd-of -self-) (fd-of -self-)))

(def special-variable *open-hci-sockets*)

(def (with-macro* e) with-hci-socket-context ()
  (assert (not (boundp '*open-hci-sockets*)) () "nesting ~S is not supported" -this-function/name-)
  (bind ((*open-hci-sockets* ()))
    (unwind-protect
         (-with-macro/body-)
      (dolist (connection *open-hci-sockets*)
        (handler-case
            (close-hci-socket connection)
          (serious-condition (e)
            (warn "Error while closing HCI-SOCKET ~A: ~A" connection e)))))))

(def (function e) open-hci-socket (&key adapter peer-address (non-blocking? t))
  "ADAPTER: mac address or a 'hci2' device name. PEER-ADDRESS: a mac address."
  (check-type adapter (or null string))
  (check-type peer-address (or null string))
  (when (and adapter
             peer-address)
    (error "~S was called with both ADAPTER and PEER-ADDRESS, provide only one." -this-function/name-))
  (bind ((device-id (cond
                      (peer-address
                       (cffi:with-foreign-object (device-address '#,bdaddr_t)
                         (c-fun/rc #,str2ba peer-address device-address)
                         (c-fun/rc #,hci_get_route device-address)))
                      (adapter
                       (c-fun/rc #,hci_devid adapter))
                      (t
                       0)))
         ((:values adapter-name mac-address) (bluez:hci/adapter-name device-id))
         (socket (c-fun/rc #,hci_open_dev device-id))
         (connection (make-instance 'hci-socket
                                    :fd socket
                                    :hci-adapter-id device-id
                                    :hci-adapter-name adapter-name
                                    :mac-address mac-address)))
    (when non-blocking?
      (setf (bluez:fd-nonblocking-p socket) t))
    (push connection *open-hci-sockets*)
    connection))

(def (function e) close-hci-socket (connection)
  (assert (member connection *open-hci-sockets*))
  (setf *open-hci-sockets* (remove connection *open-hci-sockets*))
  (c-fun/rc #,hci_close_dev (fd-of connection))
  (setf (fd-of connection) nil)
  connection)
