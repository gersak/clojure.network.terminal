(ns clojure.network.network-elements.terminal
  (:require
    [clojure.core.async :as async :refer [chan go go-loop <! onto-chan]]
    [clojure.network.network-elements.protocols :refer [Patch
                                                        download-configuration
                                                        ConfigurationManipulation
                                                        Layer1
                                                        Layer2
                                                        Layer3]]
    [clojure.network.terminal
     :as client
     :refer [is-connected?
             connect-terminal-client
             get-terminal
             disconnect]]
    [clojure.network.network-elements.ios.command-line :as ios]
    [clojure.network.network-elements.comware.command-line :as comware]
    [clojure.network.network-elements.junos.command-line :as junos]
    [clojure.network.network-elements.comware.command-line :as huawei])
  (:import [bufu.terminal.client TerminalSession]
           [java.lang UnsupportedOperationException]))

(def NOT_SUPPORTED (UnsupportedOperationException. "This funcion is not jet supported!"))

(defrecord Cisco [terminal]
  TerminalSession
  (get-terminal [_] terminal)
  Patch
  (patch [_ data]
    (do
      (assert (or (string? data) (seq data)) "Input data should be string or sequence of strings!")
      (when (seq data) (assert (every? string? data) "Every element in sequence should be string"))
      (let [commands (if (seq data) data [data])]
        (ios/enter-config-mode terminal)
        (doseq [c commands]
          (ios/send-command terminal c))
        (ios/save-configuration terminal))))
  ConfigurationManipulation
  (save-configuration [this] (ios/save-configuration this))
  (upload-configuration [_ source] (ios/copy-tftp->running source))
  (download-configuration [this destination] (ios/download-configuration this destination)))

(defrecord HP [terminal]
  TerminalSession
  (get-terminal [_] terminal)
  Patch
  (patch [this data]
    (do
      (assert (or (string? data) (seq data)) "Input data should be string or sequence of strings!")
      (when (seq data) (assert (every? string? data) "Every element in sequence should be string"))
      (let [commands (if (seq data) data [data])]
        (comware/enter-system-view terminal)
        (doseq [c commands]
          (comware/send-command terminal c))
        (comware/save-configuration terminal))))
  ConfigurationManipulation
  (save-configuration [this] (comware/save-configuration this))
  (upload-configuration [_ source] (throw NOT_SUPPORTED))
  (download-configuration [this destination] (comware/backup-configuration this destination)))


(defrecord Huawei [terminal]
  TerminalSession
  (get-terminal [_] terminal)
  Patch
  (patch [this data]
    (do
      (assert (or (string? data) (seq data)) "Input data should be string or sequence of strings!")
      (when (seq data) (assert (every? string? data) "Every element in sequence should be string"))
      (let [commands (if (seq data) data [data])]
        (huawei/enter-system-view terminal)
        (doseq [c commands]
          (huawei/send-command terminal c))
        (huawei/save-configuration terminal))))
  ConfigurationManipulation
  (save-configuration [this] (huawei/save-configuration this))
  (upload-configuration [_ source] (throw NOT_SUPPORTED))
  (download-configuration [this destination] (huawei/download-configuration this destination)))

(defrecord Juniper [terminal]
  TerminalSession
  (get-terminal [_] terminal)
  ConfigurationManipulation
  (save-configuration [this] (junos/exit-edit-mode this))
  (upload-configuration [_ destination] (throw NOT_SUPPORTED))
  (download-configuration [this destination] (junos/download-configuration this destination)))

(derive Juniper junos/os)
(derive Cisco ios/os)
(derive HP comware/os)
(derive Huawei huawei/os)

(defmulti connect-to-device (fn [host & {:keys [vendor terminal username password timeout]}] vendor))

(defmethod connect-to-device :default [host & options]
  (throw (Exception. (str "Device vendor " (pr-str (:vendor options)) " is not supported."))))

(defmethod connect-to-device :Cisco [host & {:as options}]
  (when-let [t (connect-terminal-client (assoc options :host host))]
    (when (is-connected? t)
      (when-let [router (->Cisco t)]
        (ios/terminal-length-0 router)
        router))))

(defmethod connect-to-device :HP [host & {:as options}]
  (when-let [t (connect-terminal-client (assoc options :host host))]
    (when-let [router (->HP t)]
      (comware/disable-screen-length router)
      router)))


(defmethod connect-to-device :Juniper [host & {:as options}]
  (when-let [t (connect-terminal-client (assoc options :host host))]
    (when-let [router (->Juniper t)]
      (junos/set-cli-length-unlimited router)
      router)))

(defmethod connect-to-device :Huawei [host & {:as options}]
  (when-let [t (connect-terminal-client (assoc options :host host))]
    (when-let [router (->Huawei t)]
      (huawei/screen-length router 0)
      router)))
