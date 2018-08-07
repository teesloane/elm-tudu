(ns tudu.models.todolists
  "TodoList => List"
  (:require [camel-snake-kebab.core :refer :all]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.java.jdbc :as sql]
            [schema.core :as s]
            [tudu.db :refer [db]]
            [tudu.util :as util :refer [time-now int->bool]]))

(s/defschema TodoList
  {:id            s/Str
   :name          s/Str
   :ts            s/Int
   :listType      s/Str})

(defn serialize-row
  [todo-list]
  (-> (transform-keys ->camelCaseString todo-list)))

(defn deserialize
  [todo-list]
  (transform-keys ->Camel_Snake_Case_Keyword todo-list))

(defn get-all
  []
  (sql/query db ["SELECT * FROM todolists"] {:row-fn serialize-row}))

(defn update!
  [id u-todolist]
  (sql/update! db :todolists u-todolist ["id = ?" id])
  (let [r-data (sql/query db ["SELECT * FROM todolists WHERE id = ?" id] {:row-fn serialize-row})]
    (first r-data)))

(defn create!
  "TODO try catch?"
  [todo-list]
  (let [new-id      (str "cl-" (util/time-now-ms))
        n-todo-list (-> todo-list deserialize (assoc :Id new-id))]
    (sql/insert! db :todolists n-todo-list)
    (serialize-row n-todo-list)))

(defn delete!
  [id]
  (sql/delete! db :todolists ["id = ?" id]))
