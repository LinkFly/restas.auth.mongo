(defpackage :restas.auth.mongo.example
  (:use :cl :restas.auth.mongo))

(in-package :restas.auth.mongo.example)

(defparameter *cur-storage* (make-instance 'mongo-storage))

(store-user-pass "Katya" "some-pass" *cur-storage*)
(store-user-pass "Sveta" "else-pass" *cur-storage*)
(get-user-pass "Katya" *cur-storage*)
(get-user-pass "Sveta" *cur-storage*)
(user-exist-p "blabla" *cur-storage*)
(get-users *cur-storage*)