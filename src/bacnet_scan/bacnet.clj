(ns bacnet-scan.bacnet
  (:use [hiccup.form :as form]
            [bacnet-scan.export :as exp]
            [bacnet-scan.helpfn]
            [clj-time.core :only (now)])
  (:require [clojure.repl]))

(import 'java.util.ArrayList)
(import 'java.util.List)
(import '(com.serotonin.bacnet4j 
          LocalDevice 
          RemoteDevice 
          service.acknowledgement.AcknowledgementService 
          service.acknowledgement.CreateObjectAck
          service.acknowledgement.ReadPropertyAck
          service.confirmed.ConfirmedRequestService
          service.confirmed.CreateObjectRequest
          service.confirmed.DeleteObjectRequest
          service.confirmed.ReadPropertyConditionalRequest
          service.confirmed.ReadPropertyMultipleRequest
          service.confirmed.ReadPropertyRequest
          service.confirmed.WritePropertyMultipleRequest
          service.confirmed.WritePropertyRequest
          service.confirmed.ReinitializeDeviceRequest
          service.confirmed.AtomicReadFileRequest
          service.unconfirmed.WhoIsRequest
          type.constructed.Address
          type.constructed.Destination
          type.constructed.EventTransitionBits
          type.constructed.PriorityArray
          type.constructed.PropertyReference
          type.constructed.PropertyValue
          type.constructed.ReadAccessSpecification
          type.constructed.Recipient
          type.constructed.SequenceOf
          type.constructed.WriteAccessSpecification
          type.enumerated.EngineeringUnits
          type.enumerated.ObjectType
          type.enumerated.PropertyIdentifier
          type.enumerated.Segmentation
          type.primitive.CharacterString
          type.primitive.ObjectIdentifier
          type.primitive.Real
          type.primitive.UnsignedInteger
          type.primitive.SignedInteger
          util.PropertyReferences))

(defn prop-ID-by-object-type
  "Return a sequence of property identifiers for a given object
  integer. This is not EVERY properties, but the most useful."
  [object-int]
  (let [normal-IO-pids [PropertyIdentifier/objectName 
                        PropertyIdentifier/description
                        PropertyIdentifier/presentValue
                        PropertyIdentifier/units
                        PropertyIdentifier/outOfService]
        normal-variable-pids [PropertyIdentifier/objectName 
                              PropertyIdentifier/description
                              PropertyIdentifier/presentValue]
        default-pids [PropertyIdentifier/objectName 
                      PropertyIdentifier/description
                      PropertyIdentifier/presentValue]]
    (or
     ((keyword (str object-int))
      {:0 normal-IO-pids ;analog-input
       :1 normal-IO-pids ;analog-ouput
       :2 normal-variable-pids ;analog-value
       :3 normal-IO-pids ;binary-input
       :4 normal-IO-pids ;binary-output
       :5 normal-variable-pids ;binary-value
       :8 [PropertyIdentifier/objectName ;device 
           PropertyIdentifier/description
           PropertyIdentifier/deviceType
           PropertyIdentifier/vendorIdentifier
           PropertyIdentifier/vendorName
           PropertyIdentifier/modelName]
       :10 [PropertyIdentifier/objectName  ;file
            PropertyIdentifier/description
            PropertyIdentifier/fileAccessMethod
            PropertyIdentifier/fileSize
            PropertyIdentifier/fileType]
       :13 normal-IO-pids ;multi-state-input
       :14 normal-IO-pids ;multi-state-output   
       :19 normal-variable-pids ;multi-state-value    
       :16 [PropertyIdentifier/objectName ;program
            PropertyIdentifier/description
            PropertyIdentifier/programChange 
            PropertyIdentifier/programLocation            
            PropertyIdentifier/programState]
       :17 [PropertyIdentifier/objectName ;schedule
            PropertyIdentifier/description
            PropertyIdentifier/effectivePeriod
            PropertyIdentifier/weeklySchedule
            PropertyIdentifier/scheduleDefault
            PropertyIdentifier/exceptionSchedule
            PropertyIdentifier/listOfObjectPropertyReferences]
       :20 [PropertyIdentifier/objectName
            PropertyIdentifier/description
            PropertyIdentifier/logBuffer ;tend-log
            PropertyIdentifier/logDeviceObjectProperty 
            PropertyIdentifier/loggingObject 
            PropertyIdentifier/loggingRecord 
            PropertyIdentifier/loggingType 
            PropertyIdentifier/logInterval]})
     normal-variable-pids)))


(defn get-broadcast-address
  "Return the broadcast address as a string"
  []
  (clojure.string/join "."
                       (concat
                        (take 3 (clojure.string/split
                                 (get-ip)
                                 #"\."))
                        ["255"])))

   
(defn new-local-device
  "Return a new configured BACnet local device . (A device is required
to communicate over the BACnet network.). To terminate it, use the
java method `terminate'."
  [& {:keys [device-id broadcast-address port local-address]
    :or {device-id 1337
         broadcast-address (get-broadcast-address)
         port 47808
         local-address nil}}]
  (let [ld (LocalDevice. device-id broadcast-address local-address)]
    ;(-> ld (.setMaxReadMultipleReferencesNonsegmented 14))
    (.setPort ld port)
    ld))


(defmacro with-local-device
  "Initialize a local BACnet device, execute body and terminate the
  local device. Insure that the local device won't survive beyond its
  utility and lock a port. Should be used with new-local-device."
  [[device-binding device] & body]
  (let [var (gensym)] ;create a unique error handler
  `(let [~device-binding ~device]
     (.initialize ~device-binding)
     (try ~@body
          (catch Exception ~var (str "error: " (.getMessage ~var)))
          (finally (.terminate ~device-binding))))))


(defn bac4j-to-clj
  "Check the class of the argument and transform it as needed. For
  example, #<CharacterString ...> will become a simple string, and
  #<Real ...> will be a floating number."
  [bac4j-object]
  (let [object-class (class bac4j-object)]
    (cond (= object-class com.serotonin.bacnet4j.type.primitive.Real)
          (.floatValue bac4j-object)          
          (= object-class com.serotonin.bacnet4j.type.primitive.CharacterString)
          (.toString bac4j-object)
          (= object-class com.serotonin.bacnet4j.type.primitive.Boolean)
          (if (.booleanValue bac4j-object) "true" "false")
          (or (= object-class com.serotonin.bacnet4j.type.primitive.Time)
              (= object-class com.serotonin.bacnet4j.type.primitive.Date)
              (= object-class com.serotonin.bacnet4j.type.enumerated.EngineeringUnits)
              (= object-class com.serotonin.bacnet4j.type.enumerated.FileAccessMethod))
          (.toString bac4j-object)
          (or (= object-class com.serotonin.bacnet4j.type.primitive.UnsignedInteger)
              (= object-class com.serotonin.bacnet4j.type.primitive.Unsigned8)
              (= object-class com.serotonin.bacnet4j.type.primitive.Unsigned16)
              (= object-class com.serotonin.bacnet4j.type.primitive.Unsigned32))
          (.intValue bac4j-object)
          (= object-class com.serotonin.bacnet4j.type.primitive.ObjectIdentifier)
          (.toString bac4j-object)
          true nil)))

(defn get-remote-devices-and-info
  "Given a local device, sends a WhoIs. For every device discovered,
  get its extended information. Return the remote devices as a list."
  [local-device & {:keys [min max dest-port] :or {dest-port 47808}}]
  (.sendBroadcast local-device
                  dest-port (if (and min max)
                              (WhoIsRequest.
                               (UnsignedInteger. min)
                               (UnsignedInteger. max))
                              (WhoIsRequest.)))
  (Thread/sleep 500)
  (let [rds (-> local-device (.getRemoteDevices))]
    (doseq [remote-device rds]
      (-> local-device (.getExtendedDeviceInformation remote-device)))
    rds))

(defn get-object-identifiers
  "Return a remote device's object identifiers (object-list) as a
  sequence."
  [local-device remote-device]
  (.getValues
   (-> local-device
       (.sendReadPropertyAllowNull remote-device
                                   (-> remote-device (.getObjectIdentifier))
                                   PropertyIdentifier/objectList))))

(defn get-properties-references
  "Return references for the given property identifiers sequence and
  object identifiers sequence."
  [local-device remote-device seq-object-identifier]
  (let [references (PropertyReferences.)]
    (doseq [object-identifier seq-object-identifier]
      (doseq [property-id (prop-ID-by-object-type
                           (.intValue (.getObjectType object-identifier)))]
        (-> references (.add object-identifier property-id))))
    references))


(defn object-ID-to-clj
  [object-ID]
  (let [object-type (.getObjectType object-ID)
        object-inst (.getInstanceNumber object-ID)]
    {:object-type (.toString object-type) :object-instance object-inst}))

(defn get-properties-values-for-object
  "Return a map of the property name, property number and property
  value for a single object"
  [prop-values object-ID seq-prop-ID]
  (map #(try (hash-map :prop-int (.intValue %)
                       :prop-name (.toString %)
                       :prop-value (bac4j-to-clj (.getNoErrorCheck prop-values object-ID %)))
             (catch Exception e [{:exception (str "caught exception: " (.getMessage e))}]))
       seq-prop-ID))


(defn get-properties-values-for-remote-device
  [local-device remote-device seq-object-identifiers property-references]
  (let [property-values (-> local-device
                            (.readProperties remote-device property-references))]
    (map #(hash-map :object-type
                    (.toString (.getObjectType %))
                    :object-int
                    (.intValue (.getObjectType %))
                    :object-instance
                    (.getInstanceNumber %)
                    :object-properties
                    (get-properties-values-for-object property-values
                                                      %
                                                      (prop-ID-by-object-type
                                                       (.intValue (.getObjectType %)))))
         seq-object-identifiers)))
  
(defn remote-devices-object-and-properties
  "Return a map of object and properties for the remote devices."
  [local-device remote-devices & who-is-delay]
  (let [ld local-device
        rds remote-devices
        seq-oids (map #(get-object-identifiers ld %) rds)]
    (Thread/sleep (or who-is-delay 500))
    (into {} (map (fn [rd oids]
                    (hash-map (keyword (str (.getInstanceNumber rd)))
                              {:update (.toString (now))
                               :objects (get-properties-values-for-remote-device
                                         ld
                                         rd
                                         oids
                                         (get-properties-references ld rd oids))}))
                  rds seq-oids))))
         
;; (defn -main [& args]
;;   (when-let [config (gui/query-user)]
;;     (with-local-device [ld (new-local-device config)]
;;       (let [rds (get-remote-devices-and-info
;;                  ld
;;                  :min (:lower-range config)
;;                  :max (:upper-range config)
;;                  :dest-port (:dest-port config))
;;             scan-msg (gui/scanning-bacnet-network rds)
;;             info (remote-devices-object-and-properties ld rds)]
;;         (exp/spit-to-html "Bacnet-help" info)
;;         (.dispose scan-msg)))
;;     (gui/scan-completed)))


(defn bacnet-test []
  (with-local-device [ld (new-local-device)]
                      (let [rds (get-remote-devices-and-info ld)]
                        (remote-devices-object-and-properties ld rds))))

(defn get-remote-devices-list
  "Mostly for development; return a list of remote devices ID"
  []
  (with-local-device [ld (new-local-device)]
    (.sendBroadcast ld (WhoIsRequest.))
    (Thread/sleep 500)
    (for [rd (.getRemoteDevices ld)]
      (.getInstanceNumber rd))))

(defn atomic-read-file
  "Return the file as a BACnet octet string"
  [local-device remote-device object-identifier]
  (let [properties (.readProperties local-device remote-device
                                    (get-properties-references local-device remote-device
                                                               [object-identifier]))
        file-size (.getNoErrorCheck properties object-identifier PropertyIdentifier/fileSize)
        record-access (= com.serotonin.bacnet4j.type.enumerated.FileAccessMethod/recordAccess
                         (.getNoErrorCheck properties object-identifier
                                           PropertyIdentifier/fileAccessMethod))]
    (.getFileData
     (.send local-device remote-device
            (AtomicReadFileRequest. object-identifier record-access (SignedInteger. 0) file-size)))))

    

(defn backup
  "Export the configuration files form a device."
  [local-device remote-device password]
    ;; First prepare the device (backup mode)
  (try
    (.send local-device remote-device 
           (ReinitializeDeviceRequest.
            com.serotonin.bacnet4j.service.confirmed.ReinitializeDeviceRequest$ReinitializedStateOfDevice/startbackup
            (CharacterString. password)))
    ;; Now retrieve the configuration files
    (let [config-files
          (.sendReadPropertyAllowNull local-device
                                      remote-device
                                      (ObjectIdentifier. ObjectType/device
                                                         (.getInstanceNumber remote-device))
                                      PropertyIdentifier/configurationFiles)]
      ;;export the files
      (doall ;force immediate evaluation before ending the backup procedure
       (for [cfile config-files]
         (let [file-bytes (.getBytes (atomic-read-file local-device remote-device cfile))]
           (with-open [out (java.io.FileOutputStream. (.toString cfile))]
             (.write out (byte-array file-bytes)))))))
    (catch Exception e (str "caught exception: " (.getMessage e)))
    ;; Finally exit backup mode
    (finally
     (.send local-device remote-device 
            (ReinitializeDeviceRequest.
             com.serotonin.bacnet4j.service.confirmed.ReinitializeDeviceRequest$ReinitializedStateOfDevice/endbackup
             (CharacterString. password))))))

  
