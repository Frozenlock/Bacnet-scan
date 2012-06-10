(ns bacnet-scan.bacnet
  (:use [hiccup.form :as form]
        [bacnet-scan.export :as exp]
        [bacnet-scan.helpfn]
        [clj-time.core :only (now)]
        [clojure.data.codec.base64 :as b64])
  (:require [clojure.repl]))

(import 'java.util.ArrayList)
(import 'java.util.List)
(import '(com.serotonin.bacnet4j 
          LocalDevice 
          RemoteDevice 
          service.acknowledgement.AcknowledgementService 
          service.acknowledgement.CreateObjectAck
          service.acknowledgement.ReadPropertyAck
          service.acknowledgement.ReadRangeAck
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
          service.confirmed.ReadRangeRequest
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

(def ^:dynamic local-device (new-local-device))
(def ^:dynamic remote-device (.getRemoteDevice local-device 1234))
;; dummy values. Should be bound over when running code


(defmacro with-local-device
  "Initialize a local BACnet device, execute body and terminate the
  local device. Insure that the local device won't survive beyond its
  utility and lock a port. Should be used with new-local-device."
  [new-local-device & body]
  (let [var (gensym)] ;create a unique error handler
  `(binding [local-device ~new-local-device]
     (.initialize local-device)
     (try ~@body
          (catch Exception ~var (str "error: " (.getMessage ~var)))
          (finally (.terminate local-device))))))


;; (defmacro with-local-device
;;   "Initialize a local BACnet device, execute body and terminate the
;;   local device. Insure that the local device won't survive beyond its
;;   utility and lock a port. Should be used with new-local-device."
;;   [[device-binding device] & body]
;;   (let [var (gensym)] ;create a unique error handler
;;   `(let [~device-binding ~device]
;;      (.initialize ~device-binding)
;;      (try ~@body
;;           (catch Exception ~var (str "error: " (.getMessage ~var)))
;;           (finally (.terminate ~device-binding))))))


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
  [& {:keys [min max dest-port] :or {dest-port 47808}}]
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
  [remote-device]
  (.getValues
   (-> local-device
       (.sendReadPropertyAllowNull remote-device
                                   (-> remote-device (.getObjectIdentifier))
                                   PropertyIdentifier/objectList))))

(defn get-properties-references
  "Return references for the given property identifiers sequence and
  object identifiers sequence."
  [remote-device seq-object-identifier]
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


(defn get-trend-log-data [remote-device object-identifier]
   (let [refs (PropertyReferences.)]
     (doseq [pid [PropertyIdentifier/totalRecordCount
                 PropertyIdentifier/recordCount]]
      (.add refs object-identifier pid))
    (let [prop-values (.readProperties local-device remote-device refs)
          total-record-count (.intValue
                        (.get prop-values object-identifier PropertyIdentifier/totalRecordCount))
          record-count (.intValue
                        (.get prop-values object-identifier PropertyIdentifier/recordCount))
          ref-index (+ 1 (- total-record-count record-count)) ; +1, otherwise risk an out-of-range error
          read-range-request (ReadRangeRequest.
               object-identifier PropertyIdentifier/logBuffer nil
               (com.serotonin.bacnet4j.service.confirmed.ReadRangeRequest$BySequenceNumber.
                (UnsignedInteger. ref-index)
                (SignedInteger. record-count)))
          results (.send local-device remote-device read-range-request)
          get-time (fn [data-value](.toString
                                        (org.joda.time.DateTime.
                                         (.getTimeMillis (.getTimestamp data-value)))))]
      (map #(cond (= 2 (.getChoiceType %)) ;choice :  2 = Real, 0 Log disabled buffer purged, 9 Time change
                  {:value (.toString (.getReal %)) :time (get-time %)}
                  (= 0 (.getChoiceType %))
                  {:log-status (.toString (.getLogStatus %)) :time (get-time %)}
                  (= 9 (.getChoiceType %))
                  {:time-change (.toString (.getTimeChange %)) :time (get-time %)})           
           (.getValues (.getItemData results))))))


(defn atomic-read-file-in-small-chunk [remote-device object-identifier
                                       record-access int-file-size int-file-start-position]
  (when-not (<= int-file-size int-file-start-position)
      (let [requested-count (if (>= int-file-size (+ 1500 int-file-start-position))
                          1500
                          (- int-file-size int-file-start-position))]
        (concat (.getBytes (.getFileData
                 (.send local-device remote-device
                        (AtomicReadFileRequest. object-identifier
                                                record-access
                                                (SignedInteger. int-file-start-position)
                                                (UnsignedInteger. requested-count)))))
                (atomic-read-file-in-small-chunk remote-device object-identifier
                                                 record-access int-file-size (+ 1500 int-file-start-position))))))

          
(defn atomic-read-file
  "Return the file as a BACnet octet string"
  [remote-device object-identifier]
  (let [properties (.readProperties local-device remote-device
                                    (get-properties-references remote-device
                                                               [object-identifier]))
        file-size (.getNoErrorCheck properties object-identifier PropertyIdentifier/fileSize)
        record-access (= com.serotonin.bacnet4j.type.enumerated.FileAccessMethod/recordAccess
                         (.getNoErrorCheck properties object-identifier
                                           PropertyIdentifier/fileAccessMethod))
        file-start-position 0]
    (atomic-read-file-in-small-chunk remote-device object-identifier
                                      record-access (.intValue file-size)
                                      file-start-position)))


(defn backup
  "Get the configuration files form a device as byte-arrays. If error,
  return a string describing it."
  [remote-device password]
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
         (atomic-read-file remote-device cfile))))
    (catch Exception e (str "error: " (.getMessage e)))
    ;; Finally exit backup mode
    (finally
     (try 
       (.send local-device remote-device 
              (ReinitializeDeviceRequest.
               com.serotonin.bacnet4j.service.confirmed.ReinitializeDeviceRequest$ReinitializedStateOfDevice/endbackup
               (CharacterString. password)))
       (catch Exception e (str "error: " (.getMessage e)))))))


(defn encode-base-64 [byte-array]
  (String. (b64/encode byte-array)))

(defn get-backup-and-encode 
  [remote-device password]
  (let [backup-files (backup remote-device password)]
    (if-not (string? backup-files)
      (into [] (map #(encode-base-64 (byte-array %)) backup-files)))))

(defn get-properties-values-for-remote-device
  [remote-device seq-object-identifiers property-references
   & {:keys [get-trend-log get-backup]}]
  (let [property-values (-> local-device
                            (.readProperties remote-device property-references))]
    (doall (map #(let [object-type (.toString (.getObjectType %))
                object-integer (.intValue (.getObjectType %))
                object-instance (.getInstanceNumber %)
                results (hash-map :object-type object-type
                                  :object-int object-integer
                                  :object-instance object-instance
                                  :object-properties
                                  (get-properties-values-for-object
                                   property-values %
                                   (prop-ID-by-object-type object-integer)))]
                   ; now get more specific info
            (cond (and (= object-integer 20) get-trend-log) ;trend-log
                  (assoc results :trend-log-data (get-trend-log-data remote-device %))
                  :else results))
            seq-object-identifiers))))
  
  
(defn remote-devices-object-and-properties
  "Return a map of object and properties for the remote devices."
  [remote-devices & {:keys [get-trend-log get-backup password]}]
  (let [rds remote-devices
        seq-oids (map #(get-object-identifiers %) rds)] ;delay needed?
    (into {} (map (fn [rd oids]
                    (let [prop-refs (get-properties-references rd oids)
                          objects (get-properties-values-for-remote-device
                                   rd oids prop-refs :get-trend-log get-trend-log)
                          results {:update (.toString (now))
                                   :name (.getName rd)
                                   :objects objects}]
                      (hash-map (keyword (str (.getInstanceNumber rd)))
                                (if-let [backup (and get-backup
                                                     (get-backup-and-encode rd password))]
                                  (assoc results :backup-data backup)
                                  results))))
                  rds seq-oids))))


(defn bacnet-test []
  (with-local-device (new-local-device)
    (let [rds (get-remote-devices-and-info)]
      (remote-devices-object-and-properties rds :get-trend-log false :get-backup true :password ""))))

(defn get-remote-devices-list
  "Mostly for development; return a list of remote devices ID"
  [& {:keys [local-device dest-port] :or {local-device (new-local-device) dest-port 47808}}]
  (with-local-device local-device
    (.sendBroadcast local-device dest-port (WhoIsRequest.))
    (Thread/sleep 500)
    (for [rd (.getRemoteDevices local-device)]
      (.getInstanceNumber rd))))

    
  
