(defproject bacnet-scan "1.0.2"
  :description "Small application to scan a BACnet network. Results
  are exported in an HTML file, ready to be sent to a webserver for
  further analysis."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [clj-time "0.4.2"]
                 [seesaw "1.4.0"]
                 [bacnet-scan-utils "1.0.2"]
                 [org.clojars.frozenlock/gzip64 "1.0.0"]
                 [overtone/at-at "1.0.0"]
                 [org.clojure/tools.cli "0.2.2"]]
  :main bacnet-scan.gui)
