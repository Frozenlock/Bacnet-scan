(ns bacnet-scan.gui
  (:gen-class :main true)
  (:use [clojure.string :only (split join)]
        [bacnet-scan.helpfn]
        [bacnet-scan.bacnet]
        [bacnet-scan.export :as exp]
        [seesaw.core]
        [seesaw.swingx]
        [seesaw.dev :only (show-options)]
        [seesaw.mig]))

(defn display-in-frame [frame content]
  (config! frame :content content)
  content)


(defn found-devices-widget [local-device]
  (let [remote-devices (sort (for [rd (.getRemoteDevices local-device)]
                               (str "Device "(.getInstanceNumber rd) " "
                                    (.getName rd))))]
    (scrollable (listbox :model remote-devices
                         :tip "Those are the devices found on the network"
                         :id :#rd))))

(defn scanning-bacnet-network [remote-devices]
  "Show a listbox with every remote devices found in the network"
  (native!)
  (let [f (frame :title "Bacnet network scan")
        content (mig-panel
                 :constraints ["wrap 2"
                               "[shrink 0]20px[200, grow, fill]"
                               "[shrink 0]5px[]"]
                 :items [[(str "Found " (count remote-devices) " device(s).")]
                         [(scrollable (listbox-x :model remote-devices
                                                 :sort-order :ascending
                                                 :highlighters [(hl-simple-striping)
                                                                ((hl-color :background :darkgreen) :rollover-row)]))  "span 1 2"]
                          [(busy-label :text "Scanning ..." :busy? true)]])]
    (display-in-frame f content)
    (-> f (pack!) (show!))))

(defn scan-completed []
  (native!)
  (-> (dialog :title "Scan completed!"
              :content "Scan completed!\nYou can now use the html file exported\n whenever you have an internet connection available."
              :type :info)
      (pack!)
      (show!)))


(defn parse-or-nil
  "Return the parsed string as an integer, or nil if the sting is empty"
  [string]
  (if (empty? string)
    nil
    (Integer/parseInt string)))
  

(defn query-user2 [& {:keys [on-close]}]
  (native!)
  (let [remote-devices (atom [])
        local-ip (get-ip)
        broadcast-ip (broadcast-address local-ip)
        button (button :id :scan-button
                       :text "Scan"
                       :font {:name "ARIAL" :style :bold :size 18})
        bd-address [["Broadcast address:"] [(text :id :bc-address :text broadcast-ip)]]
        local-ip [["Current IP:"] [(text :id :IP :text local-ip)]]
        scan-export (fn [ld rds]
                      (exp/spit-to-html "Bacnet-help" (remote-devices-object-and-properties ld rds))
                      (scan-completed))
        scan (listen button
                     :action (fn [e]
                               (let [devID (text (select (to-root e) [:#devID]))
                                     dest-port (text (select (to-root e) [:#dest-port]))
                                     bc-address (text (select (to-root e) [:#bc-address]))
                                     lower-range (text (select (to-root e) [:#lower-range]))
                                     upper-range (text (select (to-root e) [:#upper-range]))]
                                 (with-local-device
                                   [ld (new-local-device)]
                                   (let [rds
                                         (get-remote-devices-and-info
                                          ld
                                          :min (parse-or-nil lower-range)
                                          :max (parse-or-nil upper-range)
                                          :dest-port (parse-or-nil dest-port))
                                         remote-devices (sort (for [rd rds]
                                                                (str "Device "(.getInstanceNumber rd) " "
                                                                     (.getName rd))))]
                                     (config! (select (to-root e) [:#rd])
                                              :model remote-devices)
                                     (scan-export ld rds))))))]                                    
    (->
     (frame :title "Bacnet Network Scan"
            :on-close (or on-close :hide)
            :content
            (mig-panel
             :constraints ["wrap 2"
                           "[shrink 0]20px[300, grow, fill]"]
             :items [[button "grow, span"]
                     [:separator         "grow, span,"]
                     ["Found devices:"]
                     [(scrollable (listbox-x :model []
                                             :tip "Those are the devices found on the network"
                                             :id :rd))]
                     ["The results are exported to an html file in the same folder as this executable." "wrap, span"]
                     [:separator         "grow, span,"]
                     ["Settings" "span, center"]
                     ["Device ID: (0 to 4194303)"][(text :id :devID :text "1337")]
                     ["Range min"][(text :id :lower-range)] ["Range max"][(text :id :upper-range)]
                     ["Broadcast address:"] [(text :id :bc-address :text broadcast-ip)]
                     ["Destination port (default 47808):"][(text :id :dest-port :text "47808")]]))
     (pack!)
     (show!))))
                      

;; (defn query-user []
;;   "Open a dialog window for the user. Return this map: {:devID
;; foo, :bc-address foo, :port foo}"
;;   (native!)
;;   (let [current-ip (.getHostAddress (java.net.InetAddress/getLocalHost))
;;         possible-bc-ip (join "." (concat (take 3 (split current-ip #"\.")) ["255"]))]
;;     (->
;;      (dialog :title "BACnet configuration"
;;              :content
;;              (mig-panel
;;               :constraints ["wrap 2"
;;                             "[shrink 0]20px[200, grow, fill]"
;;                             "[shrink 0]5px[]"]
;;               :items [["Device ID: (0 to 4194303)"] [(text :id :devID)                          ]
;;                       [ "Advanced"        "split, span, gaptop 10"]
;;                       [ :separator         "growx, wrap, gaptop 10"]
;;                       ["Range min"][(text :id :lower-range)]
;;                       ["Range max"][(text :id :upper-range)]
;;                       ["Broadcast address:"       ] [(text :id :bc-address :text possible-bc-ip)]
;;                       ["Current IP:"              ] [(text :id :IP :text current-ip)            ]
;;                       ["Destination port (default 47808):"    ] [(text :id :dest-port :text "47808")]])
;;              :option-type :ok-cancel
;;              :type :question
;;              :success-fn
;;              (fn [p] {:device-id (Integer/parseInt
;;                                   (text (select (to-root p) [:#devID]))),
;;                       :broadcast-address (text (select (to-root p) [:#bc-address])),
;;                       :local-address (text (select (to-root p) [:#IP])),
;;                       :dest-port (Integer/parseInt
;;                              (text (select (to-root p) [:#dest-port])))
;;                       :lower-range (let [lr (text (select (to-root p) [:#lower-range]))]
;;                                      (when-not (empty? lr)
;;                                        (Integer/parseInt lr)))
;;                       :upper-range (let [ur (text (select (to-root p) [:#upper-range]))]
;;                                      (when-not (empty? ur)
;;                                        (Integer/parseInt ur)))}))
;;      (pack!)
;;      (show!))))



(defn remote-devices-dialog
  "A dialog showing the current remote devices"
  [local-device]
  (let [dialog (frame :title "Remote devices" :on-close :exit)]
    (config! dialog :content (listbox :model (-> local-device (.getRemoteDevices))))
    (-> dialog pack! show!)))

(defn -main [& args]
  (query-user2 :on-close :exit))
