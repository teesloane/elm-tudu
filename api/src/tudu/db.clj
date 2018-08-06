(ns tudu.db
  (:require [clojure.java.jdbc :as sql]
            [clojure.java.io :as io]
            [tudu.util :as u]))

(def db {:classname   "org.sqlite.JDBC"
         :subprotocol "sqlite"
         :subname     "data.db"})

;; -- tables --

(def tables {:todos     [[:id              :int]
                         [:is_editing      :boolean]
                         [:name            "varchar(32)"]
                         [:complete        :boolean]
                         [:parent_list     "varchar(253)"]
                         [:position        :int]
                         [:created_at      :int]
                         [:current_day     :int]
                         [:has_rolled_over :boolean]
                         [:original_day    :int]]
             :todolists [[:id             :int]
                         [:name          "varchar(256)"]
                         [:ts            :int]
                         [:list_type     "varchar(256)"]
                         [:original_name "varchar(256)"]]})

;; -- table commands --

(defn create-tables [tables]
  (sql/db-do-commands db (for [[k v] tables] (sql/create-table-ddl k v tables))))

(defn drop-tables [tables]
  (sql/db-do-commands db (for [[k v] tables] (sql/drop-table-ddl k))))

#(do (drop-tables tables)
     (create-tables tables))

(when-not (.exists (clojure.java.io/as-file "data.db"))
  (create-tables tables))
