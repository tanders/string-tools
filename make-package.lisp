(unless (find-package :string-tools)
  (make-package :string-tools
                :nicknames '(:st)
                :use '(:common-lisp)))
