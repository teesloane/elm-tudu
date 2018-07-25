(ns tudu.server
  (:require [compojure.api.sweet :refer :all]
            [compojure.route :as route]
            [ring.util.response :as resp]
            [ring.util.http-response :refer :all]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.cors :refer [wrap-cors]]
            [tudu.models.todo :as todo]
            [clojure.java.io :as io]
            ))

;; api setup

(def api-config
  {:swagger
   {:ui   "/docs-api"
    :spec "/swagger.json"
    :data {:info {:title "tudu"}
           :tags [{:name "api"}]}}})

;; middleware

(def cors
  [wrap-cors
   :access-control-allow-origin [#".*"]
   :access-control-allow-methods [:get :put :post :delete :options]])

;; Routes: Todos

(def routes-todo
  (context "/todos" [] :tags ["Todos"]
           (GET "/" request
                :summary "Returns all todos"
                :middleware [cors]
                (let []
                  (ok (todo/get-all))))

           (PATCH "/:id" [id]
                  :path-params [id :- Long]
                  :body [updated-todo todo/Todo]
                  :summary "Update a todo"
                  (ok (todo/update! id (todo/deserialize updated-todo ))))

           (POST "/" request
                 ;; :return todo/Todo
                 :body [n-todo todo/Todo]
                 :middleware [cors]
                 :summary "Creates a New Subscription"
                 (ok (todo/create! (todo/deserialize n-todo))))
           ))


;; --- Routes: Custom List ---

(def api-all 
  (api api-config
   (context "/api" [] :tags ["api"] 
     routes-todo
     )))


;; -- All e'rythin' --

;; Here we serve both the api and the front end.

(def app
  (routes
   api-all
   (GET "/" [] (io/resource "public/index.html"))
   (route/resources "/")
   (route/not-found "You lost?")))



