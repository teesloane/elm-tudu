(ns tudu.server
  (:require [compojure.api.sweet :refer :all]
            [compojure.route :as route]
            [ring.util.response :as resp]
            [ring.util.http-response :refer :all]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.cors :refer [wrap-cors]]
            [tudu.models.todo :as todo]
            [tudu.models.todolists :as todolists]
            [clojure.java.io :as io]))
            

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
             (ok (todo/update! id (todo/deserialize updated-todo))))

           (POST "/" request
             ;; :return todo/Todo
             :body [n-todo todo/Todo]
             :middleware [cors]
             :summary "Creates a New Subscription"
             (ok (todo/create! (todo/deserialize n-todo))))

           (DELETE "/:id" [id]
             :path-params [id :- Long]
             :middleware [cors]
             (todo/delete! id)
             (ok {:id id}))))


;; --- Routes: Custom List ---
(def routes-todolists
  (context "/lists" [] :tags ["Todos"]
           (GET "/" request
             :summary "Returns all todolists"
             :middleware [cors]
             (let []
               (ok (todolists/get-all))))

           (PATCH "/:id" [id]
             :path-params [id :- String]
             :body [updated-list todolists/TodoList]
             :summary "Update a todolist"
             (ok (todolists/update! id (todolists/deserialize updated-list))))

           (POST "/" request
             :body [n-lst todolists/TodoList]
             :middleware [cors]
             :summary "Creates a New List"
             (ok (todolists/create! (todolists/deserialize n-lst))))

           (DELETE "/:id" [id]
             :path-params [id :- Long]
             :middleware [cors]
             (todolists/delete! id)
             (ok {:id id}))))

(def api-all 
  (api api-config
    (context "/api" [] :tags ["api"] 
             routes-todo
             routes-todolists)))
             


;; -- All e'rythin' --

;; Here we serve both the api and the front end.

(def app
  (routes
    api-all
    (GET "/" [] (io/resource "public/index.html"))
    (route/resources "/")
    (route/not-found "You lost?")))



