(ns bufu.network-elements.huawei.comware.command-line
  (:require
    [clojure.core.async :refer (>!! <!!)]
    [bufu.terminal.client
     :as client
     :refer [send-command->
             quiz->
             async-output
             get-terminal
             async-input
             is-connected?
             disconnect
             get-remote-address
             *prompt-wrapper*
             *prompt-symbols*
             *timeout-period*]])
  (:import [bufu.terminal.client TerminalSession AsyncTerminalSession]
           [java.net ConnectException]
           [java.security PrivilegedActionException]))


(defn- NOT_SYSTEM_VIEW
  ([] (NOT_SYSTEM_VIEW nil))
  ([data] (PrivilegedActionException. (Exception. (str "Cannot do this action! Please enter \"system-view\"!\n" data)))))


(def os ::HUAWEI)


(defn- fix-credentials [cred]
  (if (= \newline (last cred))
    cred
    (apply str cred \newline)))

(defn send-command
  ([router ^String command]
   (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
   (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
   (binding [*prompt-symbols* [["<" ">"] ["\\[" "\\]"]]
             *prompt-wrapper* (fn [[b e]]
                                (re-pattern (str b ".+" e "$")))]
     (first (send-command-> (get-terminal router) command)))))

(def ^:private BREAK_STRING (String. (byte-array [0x03])))


(defn system-view? [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #"\]$" (send-command router "")))))

(defn monitor-view? [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #">$" (send-command router "")))))

(defn enter-system-view [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (when (is-connected? (get-terminal router))
    (if (system-view? router) true
      (boolean (re-find #"\]" (send-command router "system-view"))))))

(defn exit-system-view [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (when (is-connected? (get-terminal router))
    (while (system-view? router)
      (send-command router "quit"))
    true))


(defn save-configuration [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (if-let [response (quiz->
                      (get-terminal router)
                      :initial-command "save\n"
                      :quiz {"Are you sure\\?" "y\n"}
                      :break-action BREAK_STRING
                      :end-prompts ["\\]$" ">$"])]
    (boolean (re-find #"saved to device successfully" response))
    (throw (ConnectException. (str "Timeout occured while downloading configuration from: " (-> router get-terminal get-remote-address))))))

(defn commit
  "Options are:
  :trial
  :description
  :label"
  [router & options]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (if-not (system-view? router)
    (throw NOT_SYSTEM_VIEW)
    (if (seq options)
      (if-let [option (-> options
                          (select-keys [:trial :label :description])
                          seq
                          first)]
        (send-command router (str "commit " (name (key option)) " " (val option)))
        (send-command router "commit")))))


(defn startup-file [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (exit-system-view router)
  (let [info-output (send-command router "disp startup")
        startup-line (re-find #"Startup saved-configuration file.*" info-output)]
    (re-find #"(?<=cfcard:/).*" startup-line)))


(defn screen-length [router size]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (exit-system-view router)
  (send-command router (str "screen-length " size " temporary")))


(defn display-configuration [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not Huawei device."))
  (screen-length router 0)
  (let [config (send-command router "display current-configuration")
        config-lines (rest (butlast (clojure.string/split-lines config)))]
    (clojure.string/join "\r\n" config-lines)))


(defmulti download-configuration (fn [router destination] (class destination)))


(defmethod download-configuration clojure.lang.PersistentArrayMap [router remote-options]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on HuaweiOS"))
  (cond
    (contains? remote-options :tftp-server) (let [{:keys [tftp-server filename]} remote-options
                                                  tmp (str filename ".tmp")]
                                              (send-command router (str "display current-configuration > " tmp))
                                              (boolean
                                                (re-find #"Uploaded the file successfully"
                                                         (send-command router (str "tftp " tftp-server " put " tmp " " filename)))))
    (contains? remote-options :ftp-server) (throw (Exception. "FTP not supported for Huawei"))
    :else (Exception. (str "Unknown options: " remote-options))))

(defmethod download-configuration String [router filename]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on HuaweiOS"))
  (when-let [config (display-configuration router)]
    (spit filename config)
    true))

(defmethod download-configuration java.io.File [router filename]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on HuaweiOS"))
  (when-let [config (display-configuration router)]
    (spit filename config)
    true))
