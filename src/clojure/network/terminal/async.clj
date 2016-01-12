(in-ns 'clojure.network.terminal)

;; Terminal connection functions
(defn <-receive
  "Function returns string received from input-channel of terminal connection.

  There are 2 ways for function finish execution.

  First is if prompt sign or pattern is met. This is controlled with prompts and
  prompt-wrap-function.

  Prompts should be collection of strings that represent possible prompts, and
  prompt-wrap-function should be wrapper for this strings that return final regex pattern
  that will at every received input.

  Other functions that user <-receive are dependent on *prompt-symbols* and *prompt-wrapper*
  variables in bufu.terminal.client.

  Please use binding while using this functions if target prompt patterns do not match
  *prompt-symbols*"
  ([tc & {:keys [timeout-period prompts prompt-wrap-function]
          :or {timeout-period *timeout-period*
               prompts *prompt-symbols*
               prompt-wrap-function *prompt-wrapper*}}]
   (assert (satisfies? AsyncTerminalSession tc) "Terminal client \"tc\" doesn't satisfy AsyncTerminalSession protocol")
   (let [input-channel (async-input tc)]
     (if prompts
       ;; If there are prompts to check for than compare last
       ;; trimed input to prompt-patterns. If prompt-pattern found
       ;; return input
       (letfn [(prompt-check [current-response]
                 (try
                   (if (and (seq current-response) (every? seq prompts))
                     (when-let [prompt-patterns (map prompt-wrap-function prompts)]
                       (let [max-characters (apply max (map count prompts))
                             last-sane-string (apply str (take max-characters (-> current-response
                                                                                  clojure.string/trim
                                                                                  reverse)))
                             found-prompt (some #(when (re-find % current-response) %) prompt-patterns)]
                         (boolean found-prompt)))
                     false)
                   (catch Exception e
                     (do
                       (println "Error occured for response: " current-response)
                       (println "Prompt patterns are: " (pr-str prompts)) (.printStackTrace e)
                       nil))))]
         (loop [response ""]
           (let [[v c] (alts!! [input-channel (timeout timeout-period)])]
             (if (nil? v)
               ;; If timeout occured
               (throw (Exception. (str "Timeout reached while waiting for response from: " (get-remote-address tc))))
               #_response
               ;; Else if output-received
               (let [current-response (str response v)]
                 (if (prompt-check current-response)
                   current-response
                   (recur current-response)))))))
       ;; Else if there are no prompt-patterns to check for wait for idle timeout
       (loop [response ""]
         (let [[v c] (alts!! [input-channel (timeout timeout-period)])]
           (if-not (and ((comp not nil?) v) (= c input-channel))
             ;; If timeout occured
             (throw (Exception. (str "Timeout reached while waiting for response from: " (get-remote-address tc))))
             ;; Else if output-received
             (let [current-response (str response v)]
               (recur current-response)))))))))



(defn send-command-> [tc & commands]
  (assert (satisfies? AsyncTerminalSession tc) "Terminal client \"tc\" doesn't satisfy AsyncTerminalSession protocol")
  (assert (every? string? commands) "Commands have to be of String type.")
  (assert ((comp not boolean) (some (partial re-find #"\n") commands))
          "send-command-> is not intendend for sending multiline commands.
          If needed bufu.terminal.client/send-string-> can be used")
  (let [commands (for [c commands] (if-not (= \newline (last c)) (str c \newline) c))]
    (when (is-connected? tc)
      (try
        (for [c commands]
          (do
            (>!! (async-output tc) c)
            (<-receive tc)))
        (catch Exception e (do
                             (disconnect tc)
                             (close! (async-output tc))
                             (close! (async-input tc))
                             (throw e)))))))

(defn send-string->
  "Function sends non formated input strings as commands to TerminalClient
   instance. Returns list of answers for input strings.

   Beware... Because input string can contain multiline commands that could result
   in prompt offset while <-receive is executed, there is no prompt check while
   receiving. Istead only idle timeout of 5 seconds is used."
  [tc & commands]
  (assert (satisfies? AsyncTerminalSession tc) "Terminal client \"tc\" doesn't satisfy AsyncTerminalSession protocol")
  (assert (every? string? commands) "Commands have to be of String type.")
  (when (is-connected? tc)
    (try
      (for [c commands]
        (do
          (>!! (async-output tc) c)
          (<-receive tc :prompts nil)))
      (catch Exception e (do
                           (disconnect tc)
                           (close! (async-output tc))
                           (close! (async-input tc))
                           (throw e))))))

(defn quiz->
  "Quiz function is used for interction with Terminal connection. Takes input
  TerminalClient instance and based on initial command and quiz hash-map
  provides answers to terminal.

  When initial command is set, function assumes that there is no available response
  in input channel of TerminalClient. Function first enters initial-command and
  waits for connected server to respond with series of questions that should be replied
  with quiz hash-map.

  If there is no intial-command set, than function will read from input channel
  of TerminalClient and directly answer with quiz hash-map

  Example:

  {\"are u hungry?\" \"yes\"
  \"do u like pain?\" \"Saturday\"
  \"time for a raise?\" \"I quit\"}


  P.S. DON'T FORGET TO SET \\newline after answers if needed. Also initial-command"

  [tc & {:keys [initial-command quiz idle-timeout break-action end-prompts end-prompt-wrapper]
         :or {idle-timeout *timeout-period*
              end-prompt-wrapper *prompt-wrapper*
              end-prompts *prompt-symbols*}}]
  (assert (or (string? initial-command) (nil? initial-command)) "Initial command should be String type.")
  (assert (map? quiz) "quiz argument should be hash-map or some map with string keys and string vals as questions and answers")
  (assert (or (nil? break-action) (string? break-action)) "break argument should be String.")
  (assert (satisfies? AsyncTerminalSession tc) "Terminal client \"tc\" doesn't satisfy AsyncTerminalSession protocol")
  (let [complete-response (atom nil)]
    (letfn [(q&a [response question-map]
              (some #(when (re-find (re-pattern (key %)) response)
                       [(key %) (val %)]) question-map))
            (conj-new-response [old-response new-response quiz-map]
              (let [max-old (if (seq quiz-map)
                              (dec (apply max (map (comp count key) quiz-map)))
                              0)
                    new-response (str
                                   (apply str
                                          (drop (max (- (count old-response) max-old) 0) old-response))
                                   new-response)]
                new-response))]
      (let [output-channel (async-output tc)
            input-channel (async-input tc)]
        ;; If there is initial command than send it to output
        (when initial-command
          (>!! output-channel initial-command))
        (loop [[r] (when-let [[response] (alts!! [input-channel (timeout idle-timeout)])]
                     (reset! complete-response (apply str response))
                     [response])
               q quiz]
          (if (nil? r)
            ;; If idle timeout occured
            (do
              ;; Send break action
              (when break-action (>!! output-channel break-action))
              ;; QUIZ wasn NOT successfull
              ;; Don't disconnect that is somenone elses problem
              nil)
            (if (some #(re-find % r) (map end-prompt-wrapper end-prompts))
              ;; response ends with some of the end-prompts
              @complete-response
              (if-let [[question answer] (q&a r q)]
                ;; If answer found
                (do
                  ;; Send answer
                  (>!! output-channel answer)
                  ;; Reset response, to look for new question
                  (let [[response] (alts!! [input-channel (timeout idle-timeout)])
                        new-response (when response
                                       (swap! complete-response str response)
                                       [(conj-new-response r response q)])
                        new-questions (dissoc q question)]
                    (recur
                      new-response
                      new-questions)))
                ;; If no answer was found
                (do
                  (recur
                    ;; Try to get new response and recur with same questions
                    (let [[new-response] (alts!! [input-channel (timeout idle-timeout)])]
                      (when new-response
                        (swap! complete-response str new-response)
                        [(conj-new-response r new-response q)]))
                    q))))))))))


(defn bootstrap-terminal-channels
  ([terminal-client]
   {:pre [(satisfies? TerminalSession terminal-client)
          (satisfies? TerminalConnection terminal-client)
          (satisfies? TerminalIO terminal-client)]}
   (when (is-connected? terminal-client)
     (let [input-chan (async-input terminal-client)
           output-chan (async-output terminal-client)
           input-stream (-> terminal-client get-terminal get-input-stream)
           output-stream (-> terminal-client get-terminal get-output-stream )]
       ;; ASYNC input revert input stream to input-channel
       (letfn [(available? [stream]
                 (try
                   (.available stream)
                   (catch IOException e
                     0)))
               (write-output [^String out]
                 (when (is-connected? terminal-client)
                   (try
                     (.write output-stream (.getBytes out))
                     (.flush output-stream)
                     (catch java.net.SocketException e (disconnect terminal-client))
                     (catch Exception e
                       (do
                         (println "TerminalClient is connected? " (is-connected? terminal-client))
                         (println "TerminalClient connected to address " (when (is-connected? terminal-client) (get-remote-address terminal-client)))
                         (println "Error occcured while trying to write " out " to connected host")
                         (.printStackTrace e))))))
               (read-input []
                 (try
                   (if (> (available? input-stream) 0)
                     (if (> (available? input-stream) 1)
                       (try
                         (let [curr-response  (byte-array  (available? input-stream))
                               _ (.read input-stream curr-response 0  (count curr-response))
                               curr-response (String. curr-response)]
                           curr-response)
                         (catch Exception e nil))
                       (let [c (.read input-stream)]
                         (if (= -1 c)
                           nil
                           (str (char c)))))
                     "")
                   (catch Exception e (throw e))))]
         (go-loop []
           (if (is-connected? terminal-client)
             (when-let [v (read-input)]
               (if (seq v)
                 (if (>! input-chan v)
                   (recur)
                   nil)
                 (do
                   (<! (timeout 100))
                   (recur))))
             (do
               (close! input-chan)
               (disconnect terminal-client)
               nil)))
         ;; ASYNC revert output-channel to output stream
         (go-loop [out (<! output-chan)]
           (if (and out (is-connected? terminal-client))
             (do
               (write-output out)
               (recur (<! output-chan)))
             (do
               (close! output-chan)
               (disconnect terminal-client) nil)))
         true)))))


(defmulti create-terminal-client (fn [terminal-type _] terminal-type))

(defmethod create-terminal-client :default [terminal-type _]
  (throw (Exception. (str "Terminal type " (pr-str terminal-type) " is not implemented!"))))

(defmethod create-terminal-client :telnet [_ {:keys [terminal timeout] :or {timeout *connection-timeout*} :as options}]
  (let [t-client (TelnetClient.)
        input-channel (chan)
        output-channel (chan)]
    (when (try
            (.addOptionHandler t-client (TerminalTypeOptionHandler. "VT100"))
            (.addOptionHandler t-client  (WindowSizeOptionHandler.  (int 200)  (int 500) false false true true))
            (.setReaderThread t-client true)
            (.unregisterInputListener t-client)
            (.setConnectTimeout t-client timeout)
            true
            (catch Exception e nil))
      (reify
        TerminalSession
        (get-terminal [this] this)
        AsyncTerminalSession
        (async-input [_] input-channel)
        (async-output [_] output-channel)
        TerminalIO
        (get-input-stream [_] (.getInputStream t-client))
        (get-output-stream [_] (.getOutputStream t-client))
        TerminalConnection
        (connect [this address]
          (if (is-connected? this) true
            (connect this address nil)))
        (connect [this address {:keys [port username password quiz idle-timeout]
                                :or {port 23
                                     username (:username *d-login*)
                                     password (:password *d-login*)
                                     idle-timeout timeout} :as options}]

          (if (is-connected? this) true
            (when-let [client (try
                                (.setConnectTimeout t-client timeout)
                                (.connect t-client address port)
                                (. Thread (sleep 100))
                                (.isConnected t-client)
                                t-client
                                (catch ConnectException e nil)
                                (catch SocketTimeoutException e nil)
                                (catch NoRouteToHostException e nil)
                                (catch Exception e (do
                                                     (while (.isConnected t-client)
                                                       (.disconnect t-client))
                                                     nil)))]
              (try
                (when (.isConnected client)
                  (bootstrap-terminal-channels this)
                  (let [quiz (if quiz quiz
                               (let [password (if (= \newline (last password)) password (str password \newline))
                                     username (if (= \newline (last username)) username (str username \newline))]
                                 {*username-prompt* username
                                  *password-prompt* password}))
                        logged-in? (quiz-> this :quiz quiz :idle-timeout idle-timeout)]
                    (if-not logged-in?
                      (do
                        (disconnect this)
                        false)
                      true)))))))
        (get-remote-address [_]
          (-> t-client
              (.getRemoteAddress)
              (.getHostAddress)))
        (is-connected? [_]
          (if-not (.isConnected t-client) false
            (try
              (.available (.getInputStream t-client))
              true
              (catch Exception e false))))
        (disconnect [this]
          (when (is-connected? this)
            (try
              (.disconnect t-client)
              true)))))))

(defmethod create-terminal-client :ssh [_ {:keys [terminal timeout] :or {timeout *connection-timeout*} :as options}]
  (let [client (bufu.terminal.SSHShell.)
        input-channel (chan)
        output-channel  (chan)]
    (reify
      TerminalSession
      (get-terminal [this] this)
      AsyncTerminalSession
      (async-input [_] input-channel)
      (async-output [_] output-channel)
      TerminalIO
      (get-input-stream [_] (.getInputStream client))
      (get-output-stream [_] (.getOutputStream client))
      TerminalConnection
      (connect [this address]
        (if (is-connected? this) true
          (do
            (assert (map? *d-login*)
                    "Please set *d-login* if connect [address] is used without credentials.
                    SSH user is mandatory!")
            (.connect client address (merge *d-login* {:timeout timeout}))
            (bootstrap-terminal-channels this)
            true)))
      (connect [this address options]
        (if (is-connected? this) true
          (do
            (.connect client address options)
            (when (is-connected? this)
              (bootstrap-terminal-channels this)))))
      (get-remote-address [this] (.getHost client))
      (is-connected? [_] (if-not (.isConnected client) false
                              (try
                                (.available (.getInputStream client))
                                true
                                (catch Exception e false))))
      (disconnect [this]
        (when (is-connected? this)
          (try
            (.disconnect client)
            true))))))

(defn connect-terminal-client
  "Helper function. Function tries to connect to host with input parameters
  Options:
  :host
  :terminal [:ssh :telnet]
  :timeout
  :username
  :password"
  ([{:keys [host] :as options}]
   (assert (string? host) "Target :host is not specified!")
   (when-let [tc (create-terminal-client (:terminal options) options)]
     (assert (satisfies? TerminalConnection tc) (str "Client implementation for " (:terminal options)  " doesn't implement TerminalConnection protocol"))
     (assert (satisfies? TerminalIO tc) (str "Client implementation for " (:terminal options) " doesn't implement TerminalIO protocol"))
     (connect tc host options)
     (when (is-connected? tc)
       tc))))
