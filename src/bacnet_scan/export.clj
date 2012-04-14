(ns clj-bacnet.export
  (:require [hiccup.form :as form])
  (:use [hiccup.page :only (html5)]))


(def infile-css
  "<style type=\"text/css\">
body {
  background: #BDBAAF; /*#2a2b2b;*/
  color: #424550;
 /* background: url('/img/bg.png');*/
  padding: 60px 80px;
  font-family: \"HelveticaNeue-Light\", \"Helvetica Neue Light\", \"Helvetica Neue\", Helvetica, Arial, \"Lucida Grande\", sans-serif;
  font-weight: 300; /* doesn't seem to work */
}
/* Top shadow */
body:before {
content: \"\";
position: fixed;
top: -10px;
left: -10px;
width: 110%;
height: 10px;
-webkit-box-shadow: 0px 0px 15px rgba(0,0,0,.8);
-moz-box-shadow: 0px 0px 15px rgba(0,0,0,.8);
box-shadow: 0px 0px 15px rgba(0,0,0,.8);
z-index: 100;
}
/* Top shadow */
</style>
")


(defn export-to-html
  "Export the db into an hidden field in an html page, ready to be
transfered to a webserver."
  [db]
  (let [number-of-devices (count db)]
    (html5
     [:head
      [:title "BACnet helper"]
      infile-css]
     [:body
      [:div#wrapper
       [:p (if (> number-of-devices 0)
             (str "Good news! The scan revealed " number-of-devices
                  (if (> number-of-devices 1)" BACnet devices!" " BACnet device!"))
             (str "The scan revealed no BACnet devices. Perhaps you should try again?"))]
       (form/form-to ;[:post "http://173.246.15.182/devices-list"]
        [:post "http://bacnethelp.dnsd.me/devices-list"]
                     (form/hidden-field "db" db)
                     (form/submit-button "Submit to BACnet Helper!"))]])))

