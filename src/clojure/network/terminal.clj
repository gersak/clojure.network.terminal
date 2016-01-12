(ns clojure.network.terminal
  (:import [java.io InputStream OutputStream IOException]
           [java.net ConnectException SocketTimeoutException NoRouteToHostException]
           [org.apache.commons.net.telnet TelnetClient TelnetInputListener TerminalTypeOptionHandler WindowSizeOptionHandler TelnetCommand])
  (:require
    [clojure.network.terminal.SSHShell]
    [clojure.java.io :refer [input-stream output-stream]]
    [clojure.core.async :as async :refer [go-loop go >! <! timeout <!! close! >!! chan alts!! put! take!]]
    [clojure.string :refer (split-lines replace-first join split trim lower-case)]))

(def ^:dynamic *d-login* nil)
(def ^:dynamic *prompt-symbols* ["#" ">" "\\$" "\\\\" "\\]"])
(def ^:dynamic *prompt-wrapper* (fn [prompt-pattern]
                                  (when prompt-pattern
                                    (re-pattern (str "(?i)(" prompt-pattern ")\\s*$")))))

(def ^:dynamic *timeout-period* 5000)
(def ^:dynamic *username-prompt* "(?i)username")
(def ^:dynamic *password-prompt* "(?i)password")
(def ^:dynamic *connection-timeout* 8000)

(defprotocol TerminalSession
  (get-terminal [this] "Returns instance of terminal that can be used for IO device interaction"))

(defprotocol AsyncTerminalSession
  (async-input [this] "Returns instance of core.async.channel that is used to receive input from device")
  (async-output [this] "Returns instance of core.async.channel that is used to receive output from device"))

(defprotocol TerminalConnection
  "Protocol used to create Telnet and SSH
  objects that are used for information interchange."
  (connect [this address] [this address options]
  "Funciton used to connect to specific host. If no credentials
  are provided than *d-login* credentials are used.")
  (get-remote-address [this]
  "Returns remote host address")
  (is-connected? [this]
  "Return boolean response if client is connected")
  (disconnect [this]
  "Disconnects client from current host"))

(defprotocol TerminalIO
  (get-input-stream [this]
  "Returns input strem of this client instance")
  (get-output-stream [this]
  "Returns input strem of this client instance"))

(def NOT_SUPPORTED (UnsupportedOperationException. "This funcion is not jet supported!"))

(load "terminal/async")
