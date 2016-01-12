(ns clojure.network.network-elements.ios.command-line
  (:require
    [clojure.network.terminal :as client :refer [send-command->
                                                 quiz->
                                                 is-connected?
                                                 disconnect
                                                 *timeout-period*
                                                 get-terminal
                                                 get-remote-address]])
  (:import [java.net ConnectException]
           [java.security PrivilegedActionException]))

(defn- NOT_ENABLED
  ([] (NOT_ENABLED nil))
  ([data] (PrivilegedActionException. (Exception. (str "Cannot do this action! Please enter enabled-mode!\n" data)))))

(defn- fix-newline [cred]
  (if (= \newline (last cred))
    cred
    (apply str cred "\n")))

(def os ::IOS)

(defn send-command [router ^String command]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (if (-> router get-terminal is-connected?)
    (first (send-command-> (get-terminal router) command))
    (throw (ConnectException. "Not connected to router!"))))

(defn terminal-length-0 [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (boolean (send-command router "terminal length 0")))

(defn enabled? [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #"#$" (send-command router "")))))


(defn- config-mode? [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when (is-connected? (get-terminal router))
    (boolean (re-find #"\(config.*\)#$" (send-command router "")))))

(defn enter-config-mode [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (if (enabled? router)
    (if (config-mode? router) true
      (do
       (send-command router "configure terminal")
       (config-mode? router)))))

(defn exit-config-mode [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (if (config-mode? router)
    (do
     (send-command router "end")
     (not (config-mode? router)))
    true))

(defn enter-enabled-mode [router password]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (if (config-mode? router) (do (exit-config-mode router) true)
    (if (enabled? router) true
      (boolean
        (if (quiz-> (get-terminal router) :initial-command "enable\n" :quiz {"Password" (fix-newline password)} :end-prompts ["#"] :idle-timeout 1000)
          (send-command router "terminal length 0")
          ;; There was no response from quiz, or it was unsuccessfull
          ;; Check if terminal is still connected, otherwise throw error
          (when (empty? (send-command router ""))
            (throw (ConnectException. (str "Timeout occured while downloading configuration from: " (-> router get-terminal get-remote-address))))))))))


(defn save-configuration [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when (config-mode? router)
    (exit-config-mode router))
  (if (not (enabled? router))
    (throw (NOT_ENABLED (str " Saving configuration failed for host: "  (get-remote-address router))))
    (boolean (re-find #"\[OK\]" (send-command router "write")))))


(defn system-info [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when (config-mode? router) (exit-config-mode router))
  (if (not (enabled? router))
    (throw (NOT_ENABLED (str " Cannot show system info for host: " (get-remote-address router))))
    (let [sys-info (send-command router "show version")
          ios (re-find #"(?<=System image file is \").*(?=\")" sys-info)]
      {:ios ios})))

(defn copy-tftp->running
  ([router #^String tftp-host] (copy-tftp->running router tftp-host ""))
  ([router #^String tftp-host #^String tftp-path]
   (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
   (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
   (if-not (enabled? router)
     (throw (NOT_ENABLED (str " Cannot copy tftp to running for host: " (get-remote-address router))))
     (binding [*timeout-period* 30000]
       (exit-config-mode router)
       (if-let [response (quiz-> router
                                   :initial-command "copy tftp: running"
                                   :quiz {"Address or name of remote host " (fix-newline tftp-host)
                                          "Source filename " (fix-newline tftp-path)
                                          "running-config" "\n"})]
         (boolean (re-find #"bytes copied in" response))
         (when (empty? (send-command router ""))
           (throw (ConnectException. (str "Timeout occured while downloading configuration from: " (-> router get-terminal get-remote-address))))))))))




(defn copy-running->tftp
  ([router #^String tftp-host] (copy-tftp->running router tftp-host ""))
  ([{tc :terminal :as router} #^String tftp-host #^String tftp-path]
   (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
   (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
   (if-not (enabled? router)
     (throw (NOT_ENABLED (str " Cannot copy running to tftp for host " (get-remote-address tc))))
     (do
       (exit-config-mode router)
       (if-let [response (quiz-> tc
                                 :initial-command "copy running tftp:\n"
                                 :idle-timeout 30000
                                 :quiz {"Address or name of remote host" (fix-newline tftp-host)
                                        "Destination filename" (fix-newline tftp-path)})]
         (boolean (re-find #"bytes copied in" response))
         (when (empty? (send-command router ""))
           (throw (ConnectException. (str "Timeout occured while downloading configuration from: " (-> router get-terminal get-remote-address))))))))))

(defn show-configuration [router]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (if-not (enabled? router)
    (throw (NOT_ENABLED (str " Cannot show running configuration for " (get-remote-address (get-terminal router)))))
    (binding [*timeout-period* 40000]
      (when-let [configuration (send-command router "show running-config")]
        (clojure.string/join "\r\n" (rest (butlast (clojure.string/split-lines configuration))))))))

(defmulti download-configuration (fn [router destination] (class destination)))

(defmethod download-configuration clojure.lang.PersistentArrayMap [router remote-options]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (cond
    (contains? remote-options :tftp-server) (copy-running->tftp router (:tftp-server remote-options) (:filename remote-options))
    (contains? remote-options :ftp-server) (throw (Exception. "Cisco doesn't support FTP downloading."))
    :else (Exception. (str "Unknow options: " remote-options))))

(defmethod download-configuration String [router filename]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when-let [config (show-configuration router)]
    (spit filename config)
    true))

(defmethod download-configuration java.io.File [router filename]
  (assert (satisfies? clojure.network.terminal/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on IOS"))
  (when-let [config (show-configuration router)]
    (spit filename config)
    true))
