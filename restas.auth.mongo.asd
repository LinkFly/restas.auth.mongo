(asdf:defsystem :restas.auth.mongo
  :depends-on (:anaphora
               :hu.dwim.defclass-star
               :restas.auth
               :mongo-cl-driver.usocket)
  :components ((:file "restas.auth.mongo")
               ))
  