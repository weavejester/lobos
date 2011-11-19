; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.migration
  "Migrations support."
  (:use [clojure.tools.macro :only (name-with-attributes)]
        [lobos.internal :only (autorequire-backend delete execute query)])
  (:require [clojure.java.jdbc :as sql]
            [lobos.schema :as schema]
            [lobos.analyzer :as analyzer]
            [lobos.connectivity :as conn]
            [lobos.compiler :as compiler]))

(def migrations-table :lobos_migrations)

(defprotocol Migration
  "The migration protocol is meant to be reified into a single migration
  unit. See the defmigration macro."
  (id [migration])
  (up [migration])
  (down [migration]))

(defmacro migration
  "Create a new migration."
  [name & clauses]
  (let [methods (reduce
                 (fn [m [k & body]] (assoc m k body))
                 {}
                 clauses)]
    `(reify Migration
       (id [_] ~name)
       (up [_] ~@(:up methods))
       (down [_] ~@(:down methods)))))

(defonce all-migrations (atom {}))

(defmacro defmigration
  [name & args]
  (let [[name args] (name-with-attributes name args)]
    `(let [id# (str (ns-name *ns*) "/" ~(str name))]
       (def ~name (migration id# ~@args))
       (defonce ~'db-migrations (atom []))
       (swap! ~'db-migrations conj ~name)
       (swap! all-migrations assoc id# ~name))))

(defn migration-var
  [namespace]
  (-> (str namespace)
      (symbol "db-migrations")
      (find-var)))

(defn migration-ref
  [namespace]
  (if-let [v (migration-var namespace)]
    (var-get v)))

(defn list-migrations
  [namespace]
  (if-let [r (migration-ref namespace)]
    @r
    []))

(defn clear-migrations
  [namespace]
  (if-let [r (migration-ref namespace)]
    (reset! r [])))

(defn migrations-table-exists?
  [db-spec sname]
  (-> (analyzer/analyze-schema db-spec sname)
      :elements
      migrations-table))

(defn create-migrations-table
  [db-spec sname]
  (autorequire-backend db-spec)
  (when-not (migrations-table-exists? db-spec sname)
    (let [action  (schema/table migrations-table (schema/varchar :name 255))
          db-spec (assoc db-spec :schema sname)
          create-stmt (schema/build-create-statement action db-spec)]
      (execute create-stmt db-spec))))

(defn query-migrations
  [db-spec sname]
  (conn/with-connection db-spec
    (map :name (query db-spec sname migrations-table))))

(defn insert-migrations
  [db-spec sname & migrations]
  (when-not (empty? migrations)
    (sql/with-connection db-spec
      (apply
       sql/insert-rows
       (compiler/as-identifier db-spec migrations-table sname)
       (map (comp vector id) migrations)))))

(defn delete-migrations
  [db-spec sname & migrations]
  (when-not (empty? migrations)
    (conn/with-connection db-spec
      (delete db-spec sname migrations-table
              (in :name (vec (map id migrations)))))))

(defn applied-migrations [db-spec sname]
  (map all-migrations (query-migrations db-spec sname)))

(defn pending-migrations [db-spec sname namespace]
  (let [applied? (set (query-migrations db-spec sname))]
    (remove (comp applied? id)
            (list-migrations namespace))))

(defn apply-migrations
  [db-spec sname migrations]
  (doseq [migration migrations]
    (up migration)
    (insert-migrations db-spec sname migration)))

(defn rollback-migrations
  [db-spec sname migrations]
  (doseq [migration migrations]
    (down migration)
    (delete-migrations db-spec sname migration)))
