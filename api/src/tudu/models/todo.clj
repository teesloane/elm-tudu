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

(defn ex-todo
  []
  {:id              3
   :is_editing      false
   :name            "my todo"
   :complete        false
   :parent_list     "thelist"
   :position        35
   :current_day     (time-now)
   :has_rolled_over true
   :original_day    (time-now)})

;; DB Opts

(defn get-all
  []
  (sql/query db ["SELECT * FROM todos"] {:row-fn serialize-row}))

(get-all)

;; (get-all)

(defn create
  [todo]
  (let [n-todo (merge todo {:createdAt (time-now)})]
    (sql/insert! db :todos n-todo)))

(defn update!
  "TODO: build updated_at."
  [id u-todo]
  (sql/update! db :todos u-todo ["id = ?" id])
  (first (sql/query db ["SELECT * FROM todos WHERE id = ?" id] {:row-fn serialize-row}))
  )

;; (create (ex-todo))

