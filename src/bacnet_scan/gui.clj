(ns bacnet-scan.gui
  (:gen-class :main true)
  (:use [clojure.string :only (split join)]
        [bacnet-scan.helpfn]
        [bacnet-scan.bacnet]
        [bacnet-scan.export :as exp]
        [seesaw.core]
        [seesaw.swingx]
        [seesaw.dev :only (show-options)]
        [seesaw.mig]
        [seesaw.bind :only (bind)]
        [overtone.at-at]))

(defn display-in-frame [frame content]
  (config! frame :content content)
  content)

(defn valid-ip? "Check for a valid xx.xx.xx.xx IPv4 format. Does NOT
check to see if values are below 255."
  [string]
  (re-matches #"\A\d{1,3}[.]\d{1,3}[.]\d{1,3}[.]\d{1,3}\z" string))

(defn found-devices-widget []
  (let [a (atom [])
        remote-devices-list
        (listbox :model []
                 :tip "Those are the devices found on the network"
                 :id :#rd)
        my-pool (mk-pool)
        rescan (every 2000 #(reset! a (get-remote-devices-list)) my-pool)]
    (seesaw.bind/bind a (seesaw.bind/property remote-devices-list :model))
   ; (reset! a (get-remote-devices-list))
    (scrollable remote-devices-list)))
  



(defn scanning-bacnet-network [remote-devices]
  "Show a listbox with every remote devices found in the network"
  (native!)
  (let [f (frame :title "BACnethelp network scan")
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
  (let [my-pool (mk-pool)
        remote-devices (atom [])
        remote-devices-list (listbox :model []
                                     :tip "Those are the devices found on the network. The scan might reveal more."
                                     :id :#rd)
        local-ip (get-ip)
        bc-address (atom (broadcast-address local-ip))
        bc-address-text (text :id :bc-address :text @bc-address)
        rescan (every 5000
                      #(when (valid-ip? @bc-address)
                         (reset! remote-devices (get-remote-devices-list
                                                 :local-device
                                                 (new-local-device ;:device-id (parse-or-nil devID)
                                                  :broadcast-address @bc-address
                                        ;:port (parse-or-nil dest-port))
                                                  )))) my-pool)
        button (button :id :scan-button
                       :text "Scan!"
                       :font {:name "ARIAL" :style :bold :size 18})

        local-ip [["Current IP:"] [(text :id :IP :text local-ip)]]
        scan-export (fn [rds]
                      (exp/spit-to-html "Bacnet-help" (remote-devices-object-and-properties rds))
                      (scan-completed))
        scan (listen button
                     :action (fn [e]
                               (let [devID (text (select (to-root e) [:#devID]))
                                     dest-port (text (select (to-root e) [:#dest-port]))
                                     bc-address (text (select (to-root e) [:#bc-address]))
                                     lower-range (text (select (to-root e) [:#lower-range]))
                                     upper-range (text (select (to-root e) [:#upper-range]))]
                                 (with-local-device (new-local-device :device-id (parse-or-nil devID)
                                                                      :broadcast-address bc-address
                                                                      :port (parse-or-nil dest-port))
                                   (let [rds
                                         (get-remote-devices-and-info
                                          :min (parse-or-nil lower-range)
                                          :max (parse-or-nil upper-range)
                                          :dest-port (parse-or-nil dest-port))]
                                     (scan-export rds))))))]
    (seesaw.bind/bind bc-address-text bc-address)
    (seesaw.bind/bind remote-devices (seesaw.bind/property remote-devices-list :model))
    (->
     (frame :title "Bacnet Network Scan"
            :on-close (or on-close :hide)
            :content
            (mig-panel
             :constraints ["wrap 2"
                           "[shrink 0]20px[300, grow, fill]"]
             :items [[button "grow, span"]
                     [:separator         "grow, span,"]
                     ["Found devices (update 5s):"]
                     [(scrollable remote-devices-list)]
                     ["The results are exported to an html file in the same folder as this executable." "wrap, span"]
                     [:separator         "grow, span,"]
                     ["Settings" "span, center"]
                     ["Device ID: (0 to 4194303)"][(text :id :devID :text "1337")]
                     ["Range min"][(text :id :lower-range)] ["Range max"][(text :id :upper-range)]
                     ["Broadcast address:"] [bc-address-text]
                     ["Destination port (default 47808):"][(text :id :dest-port :text "47808")]
                     ["Download trendlogs"][(checkbox :id :trendlogs :text "Can take a while...")]
                     ["Download devices backups"][(checkbox :id :backups :selected? true
                                                    :text "Highly recommended")]]))
     (pack!)
     (show!))rescan))




(defn remote-devices-dialog
  "A dialog showing the current remote devices"
  [local-device]
  (let [dialog (frame :title "Remote devices" :on-close :exit)]
    (config! dialog :content (listbox :model (-> local-device (.getRemoteDevices))))
    (-> dialog pack! show!)))

(defn -main [& args]
  (query-user2 :on-close :exit))
