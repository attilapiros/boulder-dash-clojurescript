(ns my-boulder-dash.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:gen-class))

(defn render-app []
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (str "<!DOCTYPE html>"
        "<html>"
        "<head>"
        " <!-- link rel=\"stylesheet\" href=\"css/page.css\" -->"
        " <script type=\"text/javascript\">"
        "    function init() {"
        "    my_boulder_dash.init();"
        "    document.getElementById(\"myCanvas\").focus();"
        "    }"
        "</script>"

        "</head>"
        "<body onload=\"init()\">"
        "<div>"
        "<p id=\"toAdd\"></p>"
        "</div>"
	"<div>"
	"<canvas id=\"myCanvas\" width=\"500\" height=\"500\" tabindex=\"1\"></canvas>"
	"</div>"
	" <script src=\"js/cljs.js\"></script>"
        "</body>"
        "</html>")})

(defn handler [request]
  (if (= "/" (:uri request))
      (response/redirect "/help.html")
      (render-app)))

(def app
  (-> handler
    (resources/wrap-resource "public")))

(defn -main [& args]
  (jetty/run-jetty app {:port 3001}))

