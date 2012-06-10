(ns bacnet-scan.export
  (:require [hiccup.form :as form])
  (:use [hiccup.page :only (html5)]
        [gzip64.core]))

(import 'java.util.Calendar)

(def infile-css
  "<style type=\"text/css\">
body {
  background: #112424;
  color: #779898;
  width: 20em;
 /* background: url('/img/bg.png');*/
  padding: 60px 80px;
  font-family: \"HelveticaNeue-Light\", \"Helvetica Neue Light\", \"Helvetica Neue\", Helvetica, Arial, \"Lucida Grande\", sans-serif;
  font-weight: bold;
}
</style>
")

(defn get-filename
  "Given a prefix, return a filename appended with the current time in
  millisecond" [filename-prefix]
  (.getAbsolutePath
   (clojure.java.io/file
    (str filename-prefix "-"
         (.getTimeInMillis (Calendar/getInstance))
         ".html"))))


(defn export-to-html
  "Export the db into an hidden field in an html page, ready to be
transfered to a webserver."
  [filename db]
  (let [number-of-devices (count db)]
    (html5
     [:head
      [:title "BACnet Help report"]
      infile-css]
     [:body
      [:div#wrapper
       [:p (if (> number-of-devices 0)
             (str "Good news! The scan revealed " number-of-devices
                  (if (> number-of-devices 1)" BACnet devices!" " BACnet device!")
                  " If you have an Internet connection, click below to send the data to bacnethelp.com and consult it.")
             (str "The scan revealed no BACnet devices. Perhaps you should try again?"))]
       (form/form-to
                                        ;[:post "https://bacnethelp.com/devices-list"]
         [:post "https://127.0.0.1:8443/devices-list"]
                     (form/hidden-field "db" (gz64 (str db)))
                     (form/submit-button "Submit to BACnet Help!"))
       [:hr]
       [:p "No Internet connection right now? Don't worry! Simply copy this html file and bring it wherever you can reach the Internet. You can see the current location of this file by looking at your brower's address bar." ]]])))

(defn spit-to-html
  "Save the data to an html file. Return the complete file path."
  [filename-prefix data]
  (let [filename (get-filename filename-prefix)]  
    (spit filename (export-to-html filename data))
    filename))
