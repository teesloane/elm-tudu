(ns tudu.models.todo
  (:require [camel-snake-kebab.core :refer :all]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.java.jdbc :as sql]
            [schema.core :as s]
            [tudu.db :refer [db]]
            [tudu.util :refer [time-now int->bool]]))

(s/defschema Todo
  {:id            s/Int
   :isEditing     s/Bool
   :name          s/Str
   :complete      s/Bool
   :parentList    s/Str
   :position      s/Int
   :currentDay    s/Int
   :createdAt     s/Int
   :hasRolledOver s/Bool
   :originalDay   s/Int})

(defn deserialize
  [todo]
  (transform-keys ->Camel_Snake_Case_Keyword todo))

(defn serialize-row
  "Paints a happy todo to send over the wire."
  [todo]
  (-> (transform-keys ->camelCaseString todo)
      (update "isEditing" int->bool)
      (update "hasRolledOver" int->bool)
      (update "complete" int->bool)))

;; DB Opts

(defn get-all
  []
  (sql/query db ["SELECT * FROM todos"] {:row-fn serialize-row}))

(defn create!
  "TODO try catch?"
  [todo]
  (let [n-todo (-> todo deserialize (merge {:created_at (time-now)}))]
    (sql/insert! db :todos n-todo)
    (serialize-row n-todo)))

(defn update!
  "TODO: build updated_at. do try-catch?"
  [id u-todo]
  (sql/update! db :todos u-todo ["id = ?" id])
  (let [r-data (sql/query db ["SELECT * FROM todos WHERE id = ?" id] {:row-fn serialize-row})]
    (first r-data)))


(defn delete!
  [id]
  (sql/delete! db :todos ["id = ?" id]))
