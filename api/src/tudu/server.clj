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


    (OPTIONS "/:id" req :middleware [cors] :body [new-todo todo/Todo] :summary "Cors Preflight" (ok {}))
    (PATCH "/:id" [id]
      :path-params [id :- Long]
      :body [updated-todo todo/Todo]
      :summary "Updates a subscription"
      (ok (todo/update! id (todo/deserialize updated-todo ))))


    ;; ;; DELETE works, but only if there is an OPTIONS. Also, it still registers a 500 on the client :(
    ;; ;; But............................ IT WORKS.

    ;; (OPTIONS "/:id" req :middleware [cors] :summary "Cors Preflight" (ok {}))
    ;; (DELETE "/:id" [id]
    ;;   :path-params [id :- Long]
    ;;   :middleware [cors]
    ;;   (db/delete-subscription id)
    ;;   (ok {:id id}))


    ;; ;; FIXME: see todo.org/refactors/POST requests need OPTIONS
    ;; (OPTIONS "/" request
    ;;   :return Subscription
    ;;   :body [new-subscription NewSubscription]
    ;;   :middleware [cors]
    ;;   :summary "Creates a New Subscription"
    ;;   (ok {}))

    ;; (POST "/" request
    ;;   :return Subscription
    ;;   :body [new-subscription NewSubscription]
    ;;   :middleware [cors]
    ;;   :summary "Creates a New Subscription"
    ;;   (ok (db/create-subscription new-subscription)))
    ;; ))
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
   #_(GET "/" [] (resp/resource-response "index.html" {:root ""
                                                     :allow-symlinks? true
                                                     }))
   (GET "/" [] (io/resource "public/index.html"))
   (route/resources "/")
   (route/not-found "You lost?")))




