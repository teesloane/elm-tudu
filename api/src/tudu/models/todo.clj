(ns tudu.models.todo
  (:require [camel-snake-kebab.core :refer :all]
            [camel-snake-kebab.extras :refer [transform-keys]]
            [clojure.java.jdbc :as sql]
            [schema.core :as s]

            [tudu.db :refer [db]]
            [tudu.util :refer [time-now int->bool]]))

;; Examples And Serializers
#_(s/defschema Todo
    {:name s/Str
     (s/optional-key :description) s/Str
     :size (s/enum :L :M :S)
     :origin {:country (s/enum :FI :PO)
              :city s/Str}})

(s/defschema Todo
  {:id              s/Int 
   :is_editing      s/Bool
   :name            s/Str
   :complete        s/Bool
   :parent_list     s/Str
   :position        s/Int
   :current_day     s/Int
   :has_rolled_over s/Bool
   :original_day    s/Bool}
  )

#_(transform-keys ->camelCaseString {:first-name "John", :last-name "Smith"})

(defn deserialize
  [todo]
  (transform-keys ->Camel_Snake_Case_Keyword todo)
  )

(defn serialize-row
  "Paints a happy todo to send over the wire."
  [todo]
  (-> (transform-keys ->camelCaseString todo)
      (update "isEditing" int->bool)
      (update "hasRolledOver" int->bool)
      (update "complete" int->bool)

      ))

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
  (sql/update! db :todos u-todo ["id = ?" id]))

;; (create (ex-todo))



