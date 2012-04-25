(ns bacnet-scan.gui
  (:use [clojure.string :only (split join)]
        [seesaw.core]
        [seesaw.swingx]
        [seesaw.mig]))

(defn display-in-frame [frame content]
  (config! frame :content content)
  content)

(defn query-user []
  "Open a dialog window for the user. Return this map: {:devID
foo, :bc-address foo, :port foo}"
  (native!)
  (let [current-ip (.getHostAddress (java.net.InetAddress/getLocalHost))
        possible-bc-ip (join "." (concat (take 3 (split current-ip #"\.")) ["255"]))]
    (->
     (dialog :title "BACnet configuration"
             :content
             (mig-panel
              :constraints ["wrap 2"
                            "[shrink 0]20px[200, grow, fill]"
                            "[shrink 0]5px[]"]
              :items [["Device ID: (0 to 4194303)"] [(text :id :devID)                          ]
                      ["Broadcast address:"       ] [(text :id :bc-address :text possible-bc-ip)]
                      ["Current IP:"              ] [(text :id :IP :text current-ip)            ]
                      ["Port (default 47808):"    ] [(text :id :port :text "47808")]])
             :option-type :ok-cancel
             :type :question
             :success-fn (fn [p] {:device-id (Integer/parseInt
                                              (text (select (to-root p) [:#devID]))),
                                  :broadcast-address (text (select (to-root p) [:#bc-address])),
                                  :local-address (text (select (to-root p) [:#IP])),
                                  :port (Integer/parseInt
                                         (text (select (to-root p) [:#port])))}))
     (pack!)
     (show!))))

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

(defn remote-devices-dialog
  "A dialog showing the current remote devices"
  [local-device]
  (let [dialog (frame :title "Remote devices" :on-close :exit)]
    (config! dialog :content (listbox :model (-> local-device (.getRemoteDevices))))
    (-> dialog pack! show!)))
