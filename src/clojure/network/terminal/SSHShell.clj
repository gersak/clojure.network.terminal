(ns clojure.network.terminal.SSHShell
  (:import [java.io IOException]
           [ch.ethz.ssh2 Connection Session StreamGobbler ConnectionMonitor]
           [java.net ConnectException])
  (:gen-class
    :init init
    :prefix ssh-
    :state state
    :main false
    :methods [[connect [String clojure.lang.IPersistentMap] Boolean]
              [getInputStream [] java.io.InputStream]
              [getOutputStream [] java.io.OutputStream]
              [isConnected [] Boolean]
              [disconnect [] Boolean]
              [getHost [] String]]))

(defn ssh-init []
  [[] (atom {})])

(defn- get-state [this] (deref (.state this)))

(declare ssh-disconnect)

(defn- bootstrap-connection [this ^String address {:keys [username password timeout port strict-host-key-checking subsystem verifier public-key]
                                                   :or {port 22 timeout 5000 strict-host-key-checking "no"}
                                                   :as options}]
  (try
    (let [monitor (reify ConnectionMonitor
                    (connectionLost [_ _] (ssh-disconnect this)))
          conn (Connection. address port)
          ;; Use authentication agent for authentication
          ;; Sometimes authentication hangs... This is very bad rest of this API
          authentication-fn (fn [_]
                              (if public-key
                                ;; With Public key
                                (.authenticateWithPublicKey
                                  conn
                                  username
                                  (cond
                                    (string? public-key) (java.io.File. public-key)
                                    :else public-key)
                                  password)
                                ;; Or otherwise
                                (.authenticateWithPassword conn username password)))
          authentication-agent (agent nil)]
      ;; Add simple connection monitor... That shuts down connection when connection lost called
      (.addConnectionMonitor conn monitor)
      ;; Try to connect Connection
      (when (.connect conn verifier 0 timeout)
        ;; If connection was successfull
        (swap! (.state this) assoc :connection conn)
        (if (every? seq [username password])
          (if-not
            ;; Try to authenticate
            (when (await-for timeout (send-off authentication-agent authentication-fn))
              @authentication-agent)
            ;; If authentication failed
            (throw (IOException. "Authentication failed!"))
            ;; Otherwise
            (let [session (.openSession conn)]
              ;; Store session
              (swap! (.state this) assoc :session session)
              ;; Start subsystem... i.e. NETCONF
              (if subsystem (.startSubSystem session subsystem)
                (do (.requestDumbPTY session) (.startShell session)))
              ;; Store streams
              (let [in (StreamGobbler. (.getStdout session))
                    out (.getStdin session)
                    error (.getStderr session)]
                (swap! (.state this) assoc :in in :out out :error error)
                ;; Return TRUE
                true))))))
    ;; Return last known state of connection
    (boolean (:connected? (get-state this)))))


(defn ssh-connect
  "Connects Session to address with options.

  Options:
  :username
  :password
  :port
  :timeout
  :subsystem
  :strict-host-key-checking"
  ([this ^String address {:as options}]
   (when address
     (if (:connection (get-state this)) (ssh-disconnect this)
       (bootstrap-connection this address options)))))


(defn ssh-getInputStream [this]
  (when-let [in (:in (get-state this))]
    in))


(defn ssh-getOutputStream[this]
  (when-let [out (:out (get-state this))]
    out))

(defn ssh-isConnected [this]
 (try
   (boolean (:connection (get-state this)))
   (catch Exception e false)))


(defn ssh-disconnect [this]
  (let [session (:session (get-state this))
        connection (:connection (get-state this))]
    (when session (.close session))
    (when connection (.close connection))
    (reset! (.state this) {})
    true))

(defn ssh-getHost [this]
  (when-let [connection (:connection (get-state this))]
    (when (ssh-isConnected this)
      (str (.getHostname connection)))))
