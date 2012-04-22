(ns bacnet-scan.bacnet
  (:gen-class :main true)
  (:use [hiccup.form :as form]
            [bacnet-scan.gui :as gui]
            [bacnet-scan.export :as exp])
  (:require [clojure.repl]))

(import 'java.net.InetSocketAddress)
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
       :20 [PropertyIdentifier/logBuffer ;tend-log
            PropertyIdentifier/logDeviceObjectProperty 
            PropertyIdentifier/loggingObject 
            PropertyIdentifier/loggingRecord 
            PropertyIdentifier/loggingType 
            PropertyIdentifier/logInterval]})
     normal-variable-pids)))
   
(defn new-local-device
  "Return a new BACnet local device and initialize it. (A device is
required to communicate over the BACnet network.). To terminate it,
use the java method `terminate'."
  [device-ID string-broadcast-address & [string-local-bind-address]]
  (let [ld (LocalDevice. device-ID string-broadcast-address string-local-bind-address)]
    (-> ld (.setMaxReadMultipleReferencesNonsegmented 20))
    (.initialize ld)
    ld))


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
  [local-device]
  (-> local-device (.sendBroadcast (WhoIsRequest.)))
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
                          (get-properties-values-for-remote-device
                           ld
                           rd
                           oids
                           (get-properties-references ld rd oids))))
              rds seq-oids))))
         
(defn -main [& args]
  (when-let [config (gui/query-user)]
    (let [local-device (new-local-device (Integer/parseInt (:devID config))
                                         (:bc-address config)
                                         (:IP config))
          rds (get-remote-devices-and-info local-device)
          scan-msg (gui/scanning-bacnet-network rds)
          info (remote-devices-object-and-properties local-device rds)]
      (.terminate local-device)
      (spit "Bacnet-Help.html" (exp/export-to-html info))
      (.dispose scan-msg)
      (gui/scan-completed))))
  
(defn bacnet-test []
  (let [ld (new-local-device 222 "192.168.0.255" "192.168.0.3")
        rds (get-remote-devices-and-info ld)
        info (remote-devices-object-and-properties ld rds)]
    (.terminate ld)
    info))
