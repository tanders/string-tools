;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cldoc (https://gitlab.common-lisp.net/cldoc/cldoc/) -- documentation using 
;;; Just works.  Minor doc string formatting supported.
;;; Perhaps only downside: internal and exported symbols are not distinguished. Workaround: mark auxiliary function names etc. with a leading underscore (e.g., _aux-function). 
;;; BUG: Nested functions (e.g.g, global functions within flet) not documented. Could this be fixed with cldoc::define-descriptor-handler?

(require :cldoc)

(cldoc:extract-documentation 'cludg:html "/Users/torsten/common-lisp/string-tools/doc/" 
    (asdf:find-system :string-tools)
    :table-of-contents-title 
    "String Tools")

