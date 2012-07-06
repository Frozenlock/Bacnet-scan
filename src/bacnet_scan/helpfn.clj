(ns bacnet-scan.helpfn
  (:use [clojure.string :only (split join)]))

(import 'java.net.InetSocketAddress)
(import java.net.InetAddress java.net.Inet4Address)

(defmacro get-scanner-version []
  (let [x# (System/getProperty "bacnet-scan.version")]
    `~x#))

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

(defn resolve-dns
  "Return the IP of a given url, or simply return the IP unchanged"
  [IP-or-url]
  (if-not (re-matches #"\d\d?\d?\.\d\d?\d?\.\d\d?\d?\.\d\d?\d?" IP-or-url)
    (.getHostAddress
     (->> (InetAddress/getAllByName IP-or-url) 
          (filter #(instance? Inet4Address %))
          (first)))
    IP-or-url))

(defn recursive-merge [a b]
  (if (and (map? a) (map? b))
    (merge-with recursive-merge a b)
    (throw (Exception.))))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))