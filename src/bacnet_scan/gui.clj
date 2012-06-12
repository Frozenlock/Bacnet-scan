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
        [overtone.at-at])
  (:require [seesaw.bind :as b]
            [clojure.java.browse]))

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
  (let [devices-list (for [rd remote-devices]
                       (.getInstanceNumber rd))
        f (frame :title "BACnet Help network scan in action")
        content (mig-panel
                 :constraints ["wrap 2"
                               "[shrink 0]20px[200, grow, fill]"
                               "[shrink 0]5px[]"]
                 :items [[(str "Found " (count remote-devices) " device(s).")]
                         [(scrollable (listbox-x :model devices-list
                                                 :sort-order :ascending))  "span 1 2"]
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
  "Return the parsed string as an integer, or nil if there's garbage"
  [string]
  (let [result (try (Integer/parseInt string)
                    (catch Exception e))]
    (if (number? result)
      result nil)))
  

(defn query-user [& {:keys [on-close]}]
  (native!)
  (let [my-pool (mk-pool)
        remote-devices (atom [])
        remote-devices-list (listbox :model []
                                     :tip "Those are the devices found on the network. The scan might reveal more."
                                     :id :#rd)
        local-ip (get-ip)
        bc-address (atom (broadcast-address local-ip))
        bc-address-text (text :id :bc-address :text @bc-address)
        dest-port (atom 47808)
        dest-port-text (text :id :dest-port :text @dest-port)
        lower-range (atom nil)
        lower-range-text (text :id :lower-range)
        upper-range (atom nil)
        upper-range-text (text :id :upper-range)
        get-trend-logs (atom false)
        get-trend-logs-checkbox (checkbox :id :trendlogs :text "Can take a while...")
        get-backups (atom true)
        get-backups-text (checkbox :id :backups :selected? true :text "Highly recommended")
        device-id (atom 1337)
        device-id-text (text :id :devID :text (str @device-id))
        password (atom "")
        password-text (text :id :password :text @password)
        rescan-fn (fn []
                    (every 5000
                           #(reset!
                             remote-devices (get-remote-devices-list
                                             :dest-port @dest-port
                                             :local-device
                                             (new-local-device ;:device-id (parse-or-nil devID)
                                              :broadcast-address @bc-address))) my-pool))
        rescan (atom (rescan-fn))
        button (button :id :scan-button
                       :text "Scan!"
                       :font {:name "ARIAL" :style :bold :size 18}
                       :enabled? false)
        progress (progress-bar :indeterminate? true :visible? false)
        hyperlink (hyperlink :uri "https://bacnethelp.com"
                             :text "BACnet Help"
                             :font {:name "ARIAL" :size 11})
        local-ip [["Current IP:"] [(text :id :IP :text local-ip)]]
        scan-export (fn [rds]
                      (clojure.java.browse/browse-url
                       (clojure.string/replace
                        (str "file://"
                             (exp/spit-to-html "Bacnet-help" (remote-devices-object-and-properties
                                                              rds
                                                              :get-trend-log @get-trend-logs
                                                              :get-backup @get-backups
                                                              :password @password))) "\\" "/")))
        scan (listen button
                     :action (fn [e]
                               (stop @rescan)
                               (invoke-soon (config! progress :visible? true))
                               (invoke-later
                                (try (with-local-device
                                       (new-local-device :device-id @device-id
                                                         :broadcast-address @bc-address)
                                       (let [rds
                                             (get-remote-devices-and-info
                                              :min @lower-range
                                              :max @upper-range
                                              :dest-port @dest-port)]
                                         (scan-export rds)))
                                     (finally (config! progress :visible? false)
                                              (reset! rescan (rescan-fn)))))))]
    (b/bind bc-address-text (b/transform #(or (valid-ip? %) @bc-address)) bc-address)
    (b/bind dest-port-text (b/transform #(or (parse-or-nil %) @dest-port)) dest-port)
    (b/bind lower-range-text (b/transform #(or (parse-or-nil %) @lower-range)) lower-range)
    (b/bind upper-range-text (b/transform #(or (parse-or-nil %) @upper-range)) upper-range)
    (b/bind device-id-text (b/transform #(or (parse-or-nil %) @device-id)) device-id)
    (b/bind remote-devices
            (b/tee (b/bind (b/transform (fn [a] a)) (b/property remote-devices-list :model))
                   (b/bind (b/transform #(not (empty? %))) (b/property button :enabled?))))
    (b/bind password-text password)
    (let [f
          (frame :title "BACnet Help Network Scan"
                 :on-close (or on-close :hide)
                 :content
                 (mig-panel
                  :constraints ["wrap 2"
                                "[shrink 0]20px[300, grow, fill]"]
                  :items [[button "grow, span"]
                          [progress "grow, span"]
                          [:separator         "grow, span,"]
                          ["Found devices (update 5s):"]
                          [(scrollable remote-devices-list)]
                          [:separator         "grow, span,"]
                          ["Settings" "span, center"]
                          ["Device ID: (0 to 4194303)"][device-id-text]
                          ["Range min"][lower-range-text] ["Range max"][upper-range-text]
                          ["Broadcast address:"] [bc-address-text]
                          ["Destination port (default 47808):"][dest-port-text]
                          ["Download trendlogs"][get-trend-logs-checkbox]
                          ["Download devices backups"][get-backups-text]
                          ["Backup password (if any):"][password-text]
                          [:separator         "grow, span,"]
                          [hyperlink "span, align right"]]))]
      (b/bind (b/selection
               (select f [:#backups]))
              (b/tee (b/bind (b/transform (fn [a] a) ) get-backups)
                     (b/bind (b/transform (fn [a] a) ) (b/property password-text :enabled?))))
      (b/bind (b/selection (select f [:#trendlogs])) get-trend-logs)
      (-> f
          (pack!)
          (show!))) @rescan))

(defn -main [& args]
  (query-user :on-close :exit))
