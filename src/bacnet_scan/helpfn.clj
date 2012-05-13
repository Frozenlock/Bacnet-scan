(ns bacnet-scan.helpfn
  (:use [clojure.string :only (split join)]))

(import 'java.net.InetSocketAddress)

(defn get-ip
  "Return the first IPv4 address which IS NOT the localhost (\"127.0.0.1\")"
  []
  (let [IP-list
        (for [inter (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))]
          (for [ip (enumeration-seq (.getInetAddresses inter))]
            (.getHostAddress ip)))
        IPv4-list (map #(re-matches #"\d\d?\d?\.\d\d?\d?\.\d\d?\d?\.\d\d?\d?" %)
                       (flatten IP-list))]
    (first (remove #(or (= "127.0.0.1" %) (= nil %)) IPv4-list))))

(defn broadcast-address
  "Given a local-ip, return the most probable broadcast address"
  [local-ip]
  (join "." (concat (take 3 (split local-ip #"\.")) ["255"])))