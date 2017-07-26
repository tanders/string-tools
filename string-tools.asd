;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf-user)

(asdf:defsystem string-tools
  :description "A collection of definitions for processing strings and files." 
  :author "Torsten Anders <torsten.anders@beds.ac.uk>"
  :licence "GNU General Public License, Version 3"
  :version "0.1"
  :serial t ;; the dependencies are linear.
  :components ((:file "make-package")
	       (:file "string-tools")
	       (:file "export"))
  ;; :depends-on ("copy-objects")
  )



