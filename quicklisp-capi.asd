;;;; quicklisp-capi.asd

(asdf:defsystem #:quicklisp-capi
  :description "Quicklisp graphical interface"
  :author "Eric Lorenzana"
  :license "ISC"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "quicklisp-capi")))
