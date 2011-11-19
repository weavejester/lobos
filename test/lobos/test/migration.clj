;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.migration
  (:require [lobos.connectivity :as conn]
            [lobos.schema :as schema])
  (:use clojure.test
        lobos.test
        lobos.migration
        [lobos.internal :only (query)]))

(deftest test-defmigration
  (defmigration test-mig-1)
  (is (= (id test-mig-1)
         (str (ns-name *ns*) "/test-mig-1"))))

(defmacro with-migrations-table [& body]
  `(binding [*db* h2-spec]
     (conn/with-connection *db*
       (with-schema [~'lobos :lobos]
         (create-migrations-table *db* :lobos)
         ~@body))))

(deftest test-create-migrations-table
  (with-migrations-table
    (is (= (inspect-schema :elements :lobos_migrations)
           (schema/table :lobos_migrations
                         (schema/varchar :name 255)))
        "A table named 'lobos_migrations' should be created")))

(deftest test-query-migrations
  (with-migrations-table
    (is (empty? (query-migrations *db* :lobos))
        "Should return an empty list")
    (insert-migrations *db* :lobos
      (migration "foo")
      (migration "bar"))
    (is (= (query-migrations *db* :lobos)
           (list "foo" "bar"))
        "Should return a list containing 'foo' and 'bar'")))

(deftest test-insert-and-delete-migrations
  (with-migrations-table
    (insert-migrations *db* :lobos (migration "foo"))
    (is (= (query *db* :lobos :lobos_migrations)
           (list {:name "foo"}))
        "Should insert a migration entry named 'foo'")
    (insert-migrations *db* :lobos
      (migration "bar")
      (migration "baz"))
    (is (= (query *db* :lobos :lobos_migrations)
           (list {:name "foo"}
                 {:name "bar"}
                 {:name "baz"}))
        "Should insert two migration entries named 'bar' and 'baz'")
    (delete-migrations *db* :lobos (migration "foo"))
    (is (= (query *db* :lobos :lobos_migrations)
           (list {:name "bar"} {:name "baz"}))
        "Should delete a migration entry named 'foo'")
    (delete-migrations *db* :lobos
      (migration "bar")
      (migration "baz"))
    (is (empty? (query *db* :lobos :lobos_migrations))
        "Should delete all migration entries")))

(deftest test-pending-migrations
  (clear-migrations 'lobos.test.migration)
  (with-migrations-table
    (is (empty? (pending-migrations *db* :lobos 'lobos.test.migration))
        "Should return an empty list")))
