(ns bacnet-scan.cmd-line
  (:use [clojure.string :only (split join)]
        [bacnet-scan-utils.helpfn]
        [bacnet-scan-utils.bacnet]
        [bacnet-scan-utils.export :as exp]
        [clojure.tools.cli :only [cli]]
        [clojure.xml :only [emit-element]]))


;;;; Some functions to convert the clojure map to a very detailled XML ;;;;

(defn object-characteristics-to-xml [object-characs]
  (map #(hash-map :tag (key %) :attrs {:value (val %)}) object-characs))


(defn object-instances-to-xml [object-instances]
  (map #(hash-map :tag :Object-instance :attrs {:id (name (key %))}
                  :content (object-characteristics-to-xml (val %))) object-instances))


(defn object-types-to-xml [object-types]
  (map #(hash-map :tag :Object-type :attrs {:id (name (key %))}
                  :content (object-instances-to-xml (val %))) object-types))


(defn simple-map-to-xml [map-info]
  (map #(hash-map :tag (name (key %)) :attrs {:value (val %)}) map-info))


(defn devices-to-xml [devices]
  (map #(let [with-out-objects (dissoc (val %) :objects)]
          (hash-map :tag "Device" :attrs {:id (name (key %))}
                    :content (concat (object-types-to-xml (:objects (val %)))
                                     (simple-map-to-xml with-out-objects)))) devices))

(defn complete-report-to-xml [report]
  (str "<?xml version=\"1.0\"?>\n"
       (with-out-str (emit-element {:tag :Report :content (devices-to-xml (:data report))}))))

;;;; Now we get in the real stuff ;;;;

(defmacro get-app-version []
  (let [x# (System/getProperty "bacnet-scan.version")]
    `~x#))


(defn cmd-line [args]
  (let [[options args banner]
        (cli args
             ["-b" "--broadcast-address"
              "Broadcast address to send the \"Who is\""
              :default (get-broadcast-address)] ;IP as string

             ["-id" "--device-id" "The device instance for the scanner"
              :default 1337 :parse-fn #(Integer. %)]

             ["-p" "--port" "The port on which the BACnet network is listening"
              :default 47808 :parse-fn #(Integer. %)]
             
             ["-f" "--export-filename" "Export filename"
              :default (get-filename "BACnet-help" ".log")]
             
             ["-xml" "--export-xml" "Export as an XML instead of a Clojure map"
              :default false :flag true]
             
             ["-h" "--help" "Show help" :default false :flag true]

             ["-v" "--version" "Show application version" :default false :flag true])
        
        bc-add (:broadcast-address options)
        xml (:export-xml options)
        filename (:export-filename options)
        id (:device-id options)
        port (:port options)]
    
    (when (:help options)
      (println banner)
      (System/exit 0))
    (when (:version options)
      (println (get-app-version))
      (System/exit 0))

    (let [report (with-local-device (new-local-device
                                     :device-id id
                                     :broadcast-address bc-add
                                     :port port)
                   (let [rds (get-remote-devices-and-info)]
                     (remote-devices-object-and-properties rds
                                                           :get-trend-log false
                                                           :get-backup false)))]
      (spit filename (if xml (complete-report-to-xml report)
                         (str report))))))