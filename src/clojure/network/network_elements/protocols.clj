(ns clojure.network.network-elements.protocols)


(defprotocol Patch
  "Protocol used for patching network devices through terminal or any other type
   of connection."
  (patch [this data]))


(defprotocol ConfigurationManipulation
  "Protocol used for device configuration actions. Downloading,
   copying, saving, restoring, loading"
  (save-configuration [this]
  "Function used to save configuration locally")
  (upload-configuration [this destination]
  "Function used to load configuration to current running configuration")
  (download-configuration [this destination]
  "Function downloads configuration from device to destination.
  Destination can be File instance, or string URI, or hash-map with options
  {:tftp-server ip-address
   :filename example.txt}
  {:ftp ip-address
   :sftp ip-address
   :filename example.txt
   :username john
   :password doe}"))

(defprotocol Layer1
  "Protocol used for device interfaces diagnostics"
  (list-interfaces [this]
  "Returns list of interfaces that can be used for interface info. Interface description, interface name should be part of returned data")
  (interface-info [this]
  "Function returns interface info. admin-status, operational-status, counters etc."))


(defprotocol Layer2
  "Protocol used for packet switching analysis and such..."
  (list-mac-address-table [this]
  "Function used to list address binding to certain interfaces and optionaly
   dot1q encapsulation")
  (neighbours [this]
  "Returns device neighbours"))


(defprotocol Layer3
  "Protocol used for packet routing analysis"
  (list-routing-table [this] [this virtual-instance]))
