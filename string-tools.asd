;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem string-tools
  :description "A collection of definitions for processing strings and files." 
  :author "Torsten Anders"
  :version "0.1"
  :serial t ;; the dependencies are linear.
  :components ((:file "make-package")
	       (:file "string-tools")
	       (:file "export"))
  ;; :depends-on ("copy-objects")
  )



