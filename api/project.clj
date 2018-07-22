(defproject tudu "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.xerial/sqlite-jdbc "3.21.0"]
                 [org.clojure/java.jdbc "0.7.5"]
                 [camel-snake-kebab "0.4.0"]
                 [clj-http "3.7.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [ring-cors "0.1.11"]
                 [metosin/compojure-api "2.0.0-alpha18"]
                 [hiccup "1.0.5"]]
  :ring {:handler tudu.server/app}
  :profiles {:dev
             {:plugins      [[lein-ring "0.9.7"]]
              :dependencies [[javax.servlet/servlet-api "2.5"]]}})
