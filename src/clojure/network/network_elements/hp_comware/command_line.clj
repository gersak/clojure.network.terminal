(ns bufu.network-elements.hp.comware.command-line
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

(defn ^:private NOT_PRIVILEGED
  ([] (NOT_PRIVILEGED nil))
  ([data] (PrivilegedActionException. (Exception. (str "Cannot perform this operation, pleas enter super mode!\n" data)))))

(defn- NOT_SYSTEM_VIEW
  ([] (NOT_SYSTEM_VIEW nil))
  ([data] (PrivilegedActionException. (Exception. (str "Cannot do this action! Please enter \"system-view\"!\n" data)))))


(def os ::COMWARE)


(defn- fix-credentials [cred]
  (if (= \newline (last cred))
    cred
    (apply str cred \newline)))


(defn send-command
  ([router ^String command]
   (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
   (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
   (binding [*prompt-symbols* [["<" ">"] ["\\[" "\\]"]]
             *prompt-wrapper* (fn [[b e]]
                                (re-pattern (str b ".+" e "$")))]
     (first (send-command-> (get-terminal router) command)))))

(def ^:private BREAK_STRING (String. (byte-array [0x03])))



(defn system-view? [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #"\]$" (send-command router "")))))

(defn monitor-view? [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #">$" (send-command router "")))))

(defn super? [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (satisfies? bufu.terminal.client/AsyncTerminalSession (get-terminal router)) (str "router doesn't implement AsyncTerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if (system-view? router) true
    (when (is-connected? (get-terminal router))
      (binding [*prompt-symbols* [">" "\\]" "Password:.*"]]
        (let [response (first (send-command-> (get-terminal router) "super 3"))]
          (if (re-find #"(?i)password" response)
            (do
              (>!! (async-output (get-terminal router)) BREAK_STRING)
              (<!! (async-input (get-terminal router)))
              false)
            true))))))

(defn super [router password]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (if (super? router) true
      (binding [*prompt-symbols* [">" "\\]" "Password:.*"]]
        (let [response (first (send-command-> (get-terminal router) "super 3"))]
          (if (re-find #"(?i)password" response)
            (let [password-response (first (send-command-> (get-terminal router) password))]
              (if-not (re-find #"(?i)password" password-response)
                (do
                  (send-command router "screen-length disable")
                  true)
                (do
                  (send-command router BREAK_STRING)
                  false)))
            true))))))

(defn disable-screen-length [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (send-command router "system-length disable")))

(defn enter-system-view [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (if (system-view? router) true
      (if-not (super? router) (throw (NOT_PRIVILEGED (str " Cannot enter system view on " (get-remote-address router))))
        (boolean (re-find #"\]" (send-command router "system-view")))))))

(defn exit-system-view [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (when (is-connected? (get-terminal router))
    (while (system-view? router)
      (send-command router "quit"))
    true))


(defn save-configuration [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if-let [response (quiz->
                      (get-terminal router)
                      :initial-command "save\n"
                      :quiz {"Are you sure\\?" "y\n"
                             "(To leave the existing filename unchanged, press the enter key)" "\n"
                             "\\[Y/N\\]" "y\n"}
                      :break-action BREAK_STRING
                      :end-prompts ["\\]$" ">$"])]
    (boolean (re-find #"saved to device successfully" response))
    (throw (ConnectException. (str "Timeout occured while downloading configuration from: " (-> router get-terminal get-remote-address))))))


(defn display-configuration [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if-not (super? router)
    (throw (NOT_PRIVILEGED (str " Cannot display configuration on " (-> router get-terminal get-remote-address))))
    (do
      (when-not (system-view? router) (enter-system-view router))
      (clojure.string/join "\n" (butlast (clojure.string/split-lines (send-command router "display current")))))))


(defmulti backup-configuration (fn [router destination] (class destination)))


(defmethod backup-configuration clojure.lang.PersistentArrayMap [router remote-options]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if-not (super? router)
    (throw (NOT_PRIVILEGED (str " Cannot backup configuration on " (-> router get-terminal get-remote-address))))
    (cond
      (contains? remote-options :tftp-server) (do
                                                (exit-system-view router)
                                                (let [{tftp-ip :tftp-server full-path :filename} remote-options]
                                                  (assert (seq tftp-ip) "TFTP address not found. Please use input form TFTP_IP/relative_path")
                                                  (assert (seq full-path) "RELTIVE_PATH not found. Please use input form TFTP_IP/relative_path")
                                                  (binding [*timeout-period* 40000]
                                                    (when-let [response (send-command router (apply str "backup startup-configuration to " tftp-ip " " full-path))]
                                                      (boolean (re-find #"finished!" response))))))
      (contains? remote-options :ftp-server) (throw (Exception. "FTP not supported for HP."))
      :else (Exception. (str "Unknown options: " remote-options)))))

(defmethod backup-configuration String [router destination]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if-not (super? router)
    (throw (NOT_PRIVILEGED (str " Cannot backup configuration on " (-> router get-terminal get-remote-address))))
    (do
      (exit-system-view router)
      (binding [*timeout-period* 40000]
        (when-let [configuration (display-configuration router)]
          (spit destination configuration))))))


(defmethod backup-configuration java.io.File [router destination]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not HP device."))
  (if-not (super? router)
    (throw (NOT_PRIVILEGED (str " Cannot backup configuration on " (-> router get-terminal get-remote-address))))
    (do
      (exit-system-view router)
      (binding [*timeout-period* 40000]
        (when-let [configuration (display-configuration router)]
          (spit destination configuration))))))
