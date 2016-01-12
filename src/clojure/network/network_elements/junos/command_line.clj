(ns bufu.network-elements.juniper.junos.command-line
  (:require
   [bufu.terminal.client :as client :refer [send-command->
                                            quiz->
                                            get-terminal
                                            *prompt-symbols*
                                            is-connected?]])
  (:import [java.security PrivilegedActionException]
           [java.net ConnectException]))


(defn- NOT_CONNECTED
  ([] (NOT_CONNECTED nil))
  ([data] (ConnectException. (str "Disconnected from host! Cannot use this function while disconnected.\n" data))))

(def ^:private BREAK_STRING (String. (byte-array [0x03])))

(def os ::JUNOS)

(defn- os-check [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on JUNOS")))

(defn send-command [router ^String command]
  (os-check router)
  (if-not (is-connected? (get-terminal router)) (throw (NOT_CONNECTED (str "Session " (get-terminal router) " is connected? " (get-terminal router))))
    (first (send-command-> (get-terminal router) command))))

(defn set-cli-length-unlimited [router]
  (os-check router)
  (boolean (send-command router "set cli screen-length 0")))

(defn edit-mode? [router]
  (os-check router)
  (let [[prompt user-info] (-> (send-command router "")
                               clojure.string/split-lines
                               reverse)]
    (if-not user-info false
      (boolean (re-find #"\[edit\]" user-info)))))

(defn enter-edit-mode [router]
  (os-check router)
  (if (edit-mode? router) true
    (do
     (send-command router "configure private")
     (edit-mode? router))))

(defn exit-edit-mode [router]
  (os-check router)
  (if-not (edit-mode? router) true
    (do
      (send-command router "top")
      (quiz-> (get-terminal router) :initial-command "quit\n" :quiz {"Discard uncommitted changes" "y\n"} :end-prompts [">"])
      (edit-mode? router))))

(defn show-configuration
  ([router] (show-configuration router nil))
  ([router display]
   (os-check router)
   (when (edit-mode? router) (exit-edit-mode (get-terminal router)))
   (set-cli-length-unlimited router)
   (when-let [conf (case display
                     :set (send-command router "show configuration | display set")
                     :xml (let [prompt (-> router (send-command "") clojure.string/split-lines last)]
                            (binding [*prompt-symbols* #{prompt}]
                              (send-command router "show configuration | display xml")))
                     (send-command router "show configuration"))]
     (clojure.string/join "\n" (-> conf clojure.string/split-lines rest butlast butlast)))))

(defn save-configuration [router]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on JUNOS"))
  (if-not (edit-mode? router) true
    (do
     (send-command router "top")
     (send-command router "commit and-quit")
     (if (edit-mode? router)
       (do (exit-edit-mode router) false)
       true))))

(defmulti download-configuration (fn [router destination] (class destination)))


(defmethod download-configuration clojure.lang.PersistentArrayMap [_ _]
  (throw (Exception. "TFTP and FTP operations not jet supported for Junos")))

(defmethod download-configuration String [router filename]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on JUNOS"))
  (when-let [config (show-configuration router)]
    (spit (str filename) config)
    true))

(defmethod download-configuration java.io.File [router  #^java.io.File filename]
  (assert (satisfies? bufu.terminal.client/TerminalSession router) (str "router doesn't implement TerminalSession"))
  (assert (isa? (class router) os) (str "router type " (class router) " is not opperating on JUNOS"))
  (when-let [config (show-configuration router)]
    (spit (str filename) config)
    true))
