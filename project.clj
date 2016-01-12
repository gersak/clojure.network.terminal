(defproject kovacnica/clojure.network.terminal "0.1.0"
  :description "Communicate with network/equipment with terminal. Supported protocols are
               SSH and Telnet."
  :url ""
  :aot :all
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "1.0.4"]
                 [org.clojure/core.async "0.2.374"]
                 [ch.ethz.ganymed/ganymed-ssh2 "262"]
                 [kovacnica/clojure.network.ip "0.1.1"]
                 [commons-net/commons-net "3.4"]])
