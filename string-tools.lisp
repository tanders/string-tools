(in-package :string-tools)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; file content converters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file->string (filename)
  "Reads the content of a file and returns it as a string"
  (let ((array (make-array 0 
                           :element-type 'character 
                           :adjustable t 
                           :fill-pointer 0)))
    (with-open-file (s filename :direction :input)
      (do ((line (read-line s NIL :eof)
                 (read-line s NIL :eof)))
          ((eql line :eof))
        (format array "~A~%" line)))
    array))

; (file->string (ccl:choose-file-dialog))

(defun read-lisp-file (path)
  "Expects a path name to a lisp file and returns the read (but not evaluated) content of the file. For example, if the file contains (1 2 3) then the list (1 2 3) is return (i.e., not a string, but a list, but 1 is not called as a function). Note that only the 1st Lisp form in path is returned."
  (if (probe-file path)
    (with-open-file (stream path)
      (read stream))
    (warn "path ~A does not exist!" path)))


(defmacro format-to-file (path format-string &rest format-args)
  "Saves a formatstring and it's args to the given path."
  (let ((out-sym (gensym)))
    `(progn (with-open-file (,out-sym ,path
				      :direction :output
				      :if-exists :supersede)
	      (format ,out-sym ,format-string ,@format-args))
	    ,path)))

; (format-to-file "/tmp/test" "Das ist ein Test")


(defun pprint-to-file (path expr)
  "Pretty-prints to the given path."
  (with-open-file (my-stream path
                             :direction :output
                             :if-exists :supersede)
    (pprint expr my-stream))
  path)


(let* ((unix-delimiter "/")
       (mac-delimiter ":"))
  (defun unix-to-mac-path (unix-path-string)
    "Converts a path string in UNIX format into a path string in MacOS format"
    (if (equal (elt unix-path-string 0) #\/)
	;; absolute path
	(string-replace (subseq unix-path-string 1) 
			unix-delimiter mac-delimiter)
      ;; relative path
      (concatenate 'string ":"
		   (string-replace unix-path-string unix-delimiter mac-delimiter))))
  (defun mac-to-unix-path (unix-path-string)
    (if (equal (elt unix-path-string 0) #\:)
	;; relative path
	(string-replace (subseq unix-path-string 1) 
			mac-delimiter unix-delimiter)
      ;; absolute path
      (concatenate 'string "/"
		   (string-replace unix-path-string mac-delimiter unix-delimiter)))))

; (unix-to-mac-path "/test/test/") -> "test:test:"
; (unix-to-mac-path "test/test/") -> ":test:test:"

; (mac-to-unix-path "test:test:") -> "test/test/"
; (mac-to-unix-path ":test:test:") -> "/test/test/"

(let ((unix (string #\Linefeed))
      (mac (string #\Return)))
  (defun unix-to-mac-string (string)
    (string-replace string unix mac))
  (defun mac-to-unix-string (string)
    (string-replace string mac unix)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; general tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod string-position ((item string) (string string) 
                            &rest key-args &key (start 0) (end (length string)) &allow-other-keys)                            
  "Returns the index of the item string if it occurs completly in string, NIL otherwise. 
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (let ((key-args (remove-keyargs '(:start :start1 :start2 :end :end1 :end2) key-args)))
    (apply #'search item string :start2 start :end2 end key-args)))

#|
(string-position "<HTML>" "test <<HTM> <HTML> test <HTML>")
(string-position "<html>" "test <<HTM> <HTML> test <HTML>" :test #'string-equal)
(string-position "<HTML>" "test <<HTM> <HTML> test <HTML>" :from-end T)
(string-position "<HTML>" "test <<HTM> <HTML> test <HTML>" :start 13)
(string-position "TEST" "das ist ein string test" :test #'string-equal)
|#

()

(defmethod all-string-positions ((item string) (string string) &rest key-args &key (start 0) accum &allow-other-keys)
  ;; aux for verify-correct-tag-structure  
  "Returns all start positions of item in string if it occurs completly in string, NIL otherwise.
Understands keyargs :start, :end, :from-end (?) :key :test :test-not. :start and :end are related to string."
  (let ((key-args (remove-keyargs '(:start :accum) key-args))) ; !! reremoved!
    (if (= start (length string))
      accum
      (let ((pos (apply #'string-position item string :start start key-args)))
        (if (not pos)
          accum
          (apply #'all-string-positions item string :start (1+ pos) :accum (append accum (list pos)) key-args))))))

; (all-string-positions "<HTML>" "test <<HTM> <HTML> test <HTML>")
; (all-string-positions "<html>" "test <<HTM> <HTML> test <HTML>" :test #'string-equal)

(defmethod string-find ((item string) (string string) &rest key-args)
  "Returns the first found substring in string matching item. 
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string.
Current limitation: the returned substring always has the same length as item, regardless :test, :test-not or :key"
  (let ((pos (apply #'string-position item string key-args)))
    (when pos
      (subseq string pos (length item)))))

; (string-find "TEST" "das ist ein test string test" :test #'string-equal)

(defmethod string-remove ((item string) (string string) 
                          &rest key-args 
                          &key (count NIL) (end (length string)) &allow-other-keys)
  "Returns a string like string but without substrings matching item. If count is given only the first count occurences are removed.
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (if (and count (= count 0))
    string
    (let* ((key-args (remove-keyargs '(:end :count) key-args))
           (pos (apply #'string-position item string :end end key-args)))
      (if pos
        (let ((new-string (concatenate 'string 
                                       (subseq string 0 pos)
                                       (subseq string (+ pos (length item))))))
          (apply #'string-remove item new-string
                 :count (when count (1- count)) :end (- end (length item)) key-args))
        string))))

; (string-remove "TEST" "test das ist ein test string test" :test #'string-equal)

(defmethod string-count ((item string) (string string) 
                         &rest key-args  
                         &key (start 0) (end (length string)) &allow-other-keys)
  "Returns the number of substrings in string matching item. 
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (let* ((key-args (remove-keyargs '(:start) key-args)))
    (labels ((aux-func (offset count)
               (if (= offset end)
                 count
                 (let ((pos (apply #'string-position item string :start offset key-args)))
                   (if pos
                     (aux-func (1+ pos) (1+ count))
                     count)))))
      (aux-func start 0))))

; (string-count "TEST" "test das ist ein test string test" :test #'string-equal)


#| Tomporary commented -- when needed, first repair ASDF system copy-objects
(defun string-replace (string old-string new-string &rest key-args
			      &key (start 0)  &allow-other-keys)
  "Returns string by replacing occurences of old-string with new-string. Understands keyargs :start, :end, :from-end (?) :key :test :test-not. :start and :end are related to string."
  (let ((key-args (remove-keyargs '(:start) key-args))) ; !! reremoved!    
    (if (= start (length string))
	string
      (let ((pos (apply #'string-position old-string string :start start key-args)))
	(if (not pos)
	    string
	  (apply #'string-replace 
		 (replace (copy:copy string) new-string
			  :start1 pos
			  :end1 (+ pos (length old-string)))
		 old-string new-string
		 :start (1+ pos)
		 key-args))))))
|#

; (string-replace "//test//bla//" "//" "::")


(defmethod find-string-before ((item string) (string string) &rest key-args)
  "Returns substring of string before the first substring matching item encounters in string. 
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (let ((pos (apply #'string-position item string key-args)))
    (when pos
      (subseq string 0 pos))))

; (find-string-before "TEST" "test das ist ein test string test" :from-end T :test #'string-equal)

(defmethod find-string-after ((item string) (string string) &rest key-args)
  "Returns substring of string after the first substring matching item encounters in string.
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (let ((pos (apply #'string-position item string key-args)))
    (when pos
      (subseq string (+ pos (length item))))))

; (find-string-after "TEST" "test das ist ein test string" :from-end T :test #'string-equal)

(defmethod split-string ((separator character) (string string) &rest key-args &key (start 0) &allow-other-keys)
  "Splits string in a list of substrings at the given separator character (leaving separator out).
Understands keyargs :start, :end, :from-end :key :test :test-not. :start and :end are related to string."
  (when (<= start (length string))
    (let* ((key-args (remove-keyargs '(:start) key-args))
           (pos (apply #'position separator string :start start key-args))
           (end (getf key-args :end)))
      (if pos
        (cons (subseq string start pos)
              (apply #'split-string separator string :start (1+ pos) key-args))
        (let ((rest (subseq string start end)))
          (unless (string= rest "") (list rest)))))))

; (split-string #\Space "1 pi 3.14 4 5" :end 10)
; (mapcar #'read-from-string (split-string #\Space "1 pi 3.14 4 5"))

;; insert string into string at position
(defmethod insert-in-string ((item string) (string string) (position integer))
  "Inserts item in string before  position, returns an extended copy of string."
  (format NIL "~{~A~}"
          (list (subseq string 0 position)
                item
                (subseq string position))))

(defmethod insert-in-string ((items list) (string string) (positions list))
  "Inserts items in string before positions (in original string), returns an extended copy of string."
  (assert (every #'integerp positions)
          (items)
          "Not every element in positions is an integer: ~A~%While executing ~A."
          positions 'insert-in-string)
  (if (not (and items positions))
    string
    (insert-in-string (rest items) 
                      (insert-in-string (first items) string (first positions)) 
                      (mapcar #'(lambda (pos)
                                  (+ pos (length (first items))))
                              (rest positions)))))

; (position #\e (insert-in-string "ein " "das ist test" 8))
; (insert-in-string '("ist " "ein ") "das noch test" '(4 9))
; (insert-in-string '("1" "2" "3") "" '(0 0 0))
; (insert-in-string '("abc" " " "abc") "" '(0 0 0))


(defmethod remove-within-positions ((string string) (start integer) (end integer))
  "Removes substring from start to before end out of string"
  (format NIL "~A~A" (subseq string 0 start) (subseq string end)))

; (remove-within-positions "Das ist ein Test string" 10 20)

(defmethod find-first-in-string ((items list) (string string) &rest key-args)
  "Returns the first items string found in string."
  (assert (every #'stringp items)
          (items)
          "Not every element in ~A is a string.~%While executing ~A."
          items 'find-first-in-string)
  (let* ((positions-in-string
          (mapcan #'(lambda (item)
		      (let ((pos (apply #'string-position
					item string key-args)))
			(when pos
			  (list pos))))
                  items))
         (min (apply #'min positions-in-string))
         (positon-in-items (when min (position min positions-in-string))))
    (format T "~%debug: positions-in-string: ~A, min: ~A, positon-in-items: ~A"
	    positions-in-string min positon-in-items)
    (when positon-in-items
      (nth positon-in-items items))))

; (find-first-in-string '("<a>" "<b>" "<c>") "test <b> <a> <c>")

(defmethod white-space ((n integer))
  "Outputs a string of n white spaces"
  ;; could be more efficient...
  (format NIL "~{~A~}"
	  (loop for i from 1 to n
		collect (format NIL " "))))
	  
; (white-space 3)

(defmethod indent-string ((string string) (n integer))
  "Inserts n white spaces at the beginning and before each new line of string"
  (let ((space (white-space n))
	(lines (split-string #\Linefeed string)))
    (if lines 
	(format nil "~A~A~{~A~}"
		;; first line extra (to avoid an additional line break)
		space (first lines)
		(mapcar #'(lambda (s)
			    (format nil "~%~A~A" space s))
			(rest lines)))
      ;; if string is ""
      space)))

; (indent-string "" 4)
; (indent-string "x" 4)
; (indent-string (format NIL "test~%test~%test") 4)

(defun stringify (thing)
  "Returns a string surrounded by escaped double quotes"
  (format NIL "\"~A\"" thing))

; (stringify "test")
			  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tag related
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-string-within-tags ((start-tag string) (end-tag string) (string string) 
                                    &key (start 0) (end (length string)) (case-sensitive? NIL))
  "Returns substring of string embraced by the start and end tag, excluding these tags.
Second return value is the list of start and end position of the embraced string inside string, including the tags."
  (flet ((get-pos (tag &rest key-args)
           (let ((test-func (if case-sensitive? #'string= #'string-equal)))
             (apply #'string-position tag string :test test-func key-args))))
    (let ((pos1 (get-pos start-tag :start start :end end)))
      (when pos1
        (let ((pos2 (get-pos end-tag :start (+ pos1 (length start-tag)) :end end)))
          (when pos2
            (values (subseq string 
                            (+ pos1 (length start-tag))  
                            pos2)
                    (list pos1 (+ pos2 (length end-tag))))))))))


#|
(find-string-within-tags "<!--" "-->" "test <!-- :start :spec --> test <!-- :end :spec --> testen")
(find-string-within-tags "Test1" "Test2" " test1 x y z test2")
(find-string-within-tags "<!--" "-->" "test <!-- :start :spec --> test <!-- :end :spec --> testen" :start 10)
(find-string-within-tags "<!--" "-->" "test <!-- :start :spec --> test <!-- :end :spec --> testen" :start 35 :from-end T)
(find-string-within-tags "<!--" "-->" "test <!-- <!-- :start :spec --> test")
|#

#|
(defmethod find-string-within-tags ((start-tag string) (end-tag string) (file pathname) &rest args)
  "Returns substring of file content (as string) embraced by the start and end tag, excluding these tags.
Second return value is the list of start and end position of the embraced string inside string, including the tags."
  (apply #'find-string-within-tags start-tag end-tag (file->string file) args))
|#


#|
(find-string-within-tags "<!-- :start :spec -->" "<!-- :end :spec -->"  #P"Torsten HD:Desktop Folder:dummy.html")
(find-string-within-tags "<!-- :start :spec -->" "<!-- :end :spec -->"  #P"Torsten HD:Desktop Folder:dummy.html"
                         :start 200)

|#

#|
(defmethod all-start-end-positions ((start-tag string) (end-tag string) (string string) 
                                    &key (case-sensitive NIL))
  "Returns all start end position pairs of start-tag/end-tag in string, NIL otherwise.
The same tag pairs may occure several times, one after another or nested."
  (let* ((test-func (if case-sensitive #'string= #'string-equal))
         (start-positions (all-string-positions start-tag string :test test-func))
         (end-positions (all-string-positions end-tag string :test test-func)))
    (unless (= (length start-positions) (length end-positions))
      (error "The number of occurences of the start tag ~A and end tag ~A does not match." start-tag end-tag))
    (reverse
     (all-start-end-positions-aux start-tag end-tag string 
                                  ;; to handle nested tags with the same name: start with the last one
                                  :start-positions (reverse start-positions)
                                  :test-func test-func))))
|#

(defmethod all-start-end-positions ((start-tag string) (end-tag string) (string string) 
                                    &key (case-sensitive NIL))
  "Returns all start end position pairs of start-tag/end-tag in string, NIL otherwise.
The same tag pairs may occure several times, one after another or nested."
  (let* ((test-func (if case-sensitive #'string= #'string-equal))
         (start-positions (all-string-positions start-tag string :test test-func))
         (end-positions (all-string-positions end-tag string :test test-func)))
    (unless (= (length start-positions) (length end-positions))
      (error "The number of occurences of the start tag ~A and end tag ~A does not match." start-tag end-tag))
    (reverse 
     (all-start-end-positions-aux (reverse start-positions) 
                                  end-positions
                                  :following-start (length string)
                                  :start-tag start-tag
                                  ))))

; (all-start-end-positions "<b>" "</b>" "<b><b></b></b>") --> ((0 10) (3 6))
; (all-start-end-positions "<b>" "</b>" "<b></b><b></b>") --> ((0 3) (7 10))
; (all-start-end-positions "<b>" "</b>" "</b><b><b></b>") --> error
; (all-start-end-positions "<b>" "</b>" "<b></b></b></b>") --> error
;; Fehler:
; (all-start-end-positions "#|" "|#"  "#| 1 #| 2 |# #| 3 |# 4 |#") --> ((0 23) (5 10) (13 18)) 

#|
(defmethod all-start-end-positions-aux ((start-tag string) (end-tag string) (string string) 
                                        &key test-func start-positions  (following-start (length string)) 
                                        (following-end (first start-positions)) 
                                        accum)
  (if (null start-positions)
    accum
    (let ((end-pos (or (string-position end-tag string          ; the proceeding start tag needs to find its end before the following start 
                                        :start (1+ (first start-positions))
                                        :end following-start
                                        :test test-func)
                       (string-position end-tag string          ; of after the following end
                                        :start (1+ following-end) 
                                        :test test-func))))
      (if (not end-pos)
        (error "For start tag ~A no corresponding end tag found." start-tag)
        (all-start-end-positions-aux start-tag end-tag string 
                                     :start-positions (rest start-positions)
                                     :following-start (first start-positions)
                                     :following-end end-pos
                                     :test-func test-func
                                     :accum (append accum 
                                                    (list (list (first start-positions) end-pos))))))))
|#

;; to handle nested tags with the same name: start with the last one
(defun all-start-end-positions-aux (start-positions end-positions
                                                    &key following-start start-tag
                                                    (following-end (first start-positions)) 
                                                    accum)
  (if (null start-positions)
    accum
    (let* ((start-pos (first start-positions))
          (end-pos (find-if #'(lambda (x)
                                (or (<= (1+ start-pos) x following-start)       ; the proceeding start tag needs to find its end before the following start 
                                    (<= (1+ following-end) x)))         ; or after the following end
                            end-positions)))
      (if (not end-pos)
        (error "For start tag ~A no corresponding end tag found." start-tag)
        (all-start-end-positions-aux (rest start-positions)
                                     (remove end-pos end-positions)
                                     :following-start (first start-positions)
                                     :following-end end-pos
                                     :start-tag start-tag
                                     :accum (append accum 
                                                    (list (list (first start-positions) end-pos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; utils
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-property (property property-list)
  "Removes a property and its value out of a property list"
  (let ((pos (position property property-list)))
    (if pos
      (append (subseq property-list 0 pos)
              (subseq property-list (+ pos 2)))
      property-list)))

(defun remove-keyargs (to-remove key-args)
  (reduce #'(lambda (l p) (remove-property p l)) 
          to-remove :initial-value key-args))

; (remove-property :test '(:a 1 :test 2 :x 3))

