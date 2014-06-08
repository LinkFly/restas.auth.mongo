;(ql:quickload :anaphora)
;(ql:quickload :hu.dwim.defclass-star)
;(ql:quickload :restas.auth)
;(ql:quickload :mongo-cl-driver.usocket)

(defpackage :restas.auth.mongo
  (:use :cl :mongo :anaphora :restas.auth.storage :hu.dwim.defclass-star)
  (:import-from :mongo #:son)
  (:export #:mongo-storage #:store-user-pass #:get-user-pass #:get-users #:user-exist-p))

(in-package :restas.auth.mongo)

(defparameter *default-hostname* "localhost")
(defparameter *default-port* 27017)
(defparameter *default-users-db* "mongo-cl-users")
(defparameter *default-users-collection* "users")

;;;;;;;;;;;; Internal ;;;;;;;;;;;;;
(defparameter *write-concern* mongo::+write-concern-journal+)

(defun get-mongo-db (&key (hostname *default-hostname*)
                          (port *default-port*)
                          (db-name *default-users-db*))
  (make-instance 'mongo:database
                 :name db-name
                 :mongo-client (create-mongo-client :usocket
                                                    :write-concern *write-concern*
                                                    :server (make-instance 'mongo:server-config
                                                                           :hostname hostname
                                                                           :port port
                                                                           ))))
;(defparameter *cur-db* (get-mongo-db))

(defun get-users-collection (db &optional (users-collection *default-users-collection*))
  (mongo:collection db users-collection))
;(defparameter *users-col* (get-users-collection *cur-db*))

(defun get-user (col user)
  (mongo:find-one col
                  :query (mongo::son "user" user)))

(defun save-user (col user pass)
  (aif (get-user col user)
       (mongo:update-op col 
                        (mongo::son "user" user)
                        (progn 
                          (setf (gethash "pass" it) pass)
                          it)
                        :write-concern *write-concern*)
       (mongo:insert-op col 
                        (mongo::son "user" user "pass" pass)
                        :write-concern *write-concern*)))
;(save-user *users-col* "LinkFly" "777")
;(save-user *users-col* "Mike" "666")

(defun get-all-users (col)
  (mapcar (lambda (hash) (gethash "user" hash))
          (mongo:find-list col :query (mongo::son))))
;(get-all-users *users-col*)



(defun get-user-password (col user)
  ;(setf user "LinkFly")
  (aif (get-user col user) 
       (gethash "pass" it)))
;(get-user-password *users-col* "LinkFly")

;;;; Implementation based on mongodb
(defclass* mongo-storage (storage)
           ((hostname *default-hostname*)
            (port *default-port*)
            (dbname *default-users-db*)
            (users-collection *default-users-collection*)
            (db)
            (users-col)))

(defmethod shared-initialize :after ((storage  mongo-storage) slot-names &rest initargs &key)
  (with-slots (hostname port dbname users-collection db users-col)
      storage
    (setf db (get-mongo-db :hostname hostname :port port :db-name dbname))
    (setf users-col (get-users-collection db users-collection))))
;(defparameter storage (make-instance 'mongo-storage))
           
(defmethod store-user-pass (user pass (storage mongo-storage) &aux data pathname)
  (save-user (users-col-of storage) user (maybe-hashing-password pass storage)))

(defmethod get-user-pass (user (storage mongo-storage))
  (get-user-password (users-col-of storage) user))
;(get-user-pass "LinkFly" storage)
      
(defmethod get-users ((storage mongo-storage))
  (get-all-users (users-col-of storage)))
;(get-users storage)
  
(defmethod user-exist-p (user (storage mongo-storage))
  (and (get-user-pass user storage) t))
;(user-exist-p "LinkFly" storage)
