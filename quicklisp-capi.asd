;;;; quicklisp-capi.asd
;;
;;;; Copyright (c) 2019 Eric Lorenzana <eric.lorenzana@protonmail.ch>

(asdf:defsystem #:quicklisp-capi
  :description "Quicklisp graphical interface"
  :author "Eric Lorenzana"
  :license "TODO"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "quicklisp-capi")))
