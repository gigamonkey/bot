(in-package :com.gigamonkeys.bot)

(defparameter *blanks* (list #\Space #\Tab #\Return #\Newline))

(defparameter *files* 
  '((#P"AI.aiml" :ok)
    (#P"ALICE.aiml" :ok)
    (#P"Adverbs.aiml" :ok)
    (#P"Astrology.aiml" :ok)
    (#P"Atomic.aiml" :ok)
    (#P"Badanswer.aiml" :special-tags)
    (#P"Biography.aiml" :ok)
    (#P"Blackjack.aiml" :ok)
    (#P"Bot.aiml" :ok)
    (#P"Botmaster.aiml" :ok)
    (#P"Client.aiml" :ok)
    (#P"Computers.aiml" :ok)
    (#P"Date.aiml" :ok)
    (#P"Default.aiml" :ok)
    (#P"Dialog.aiml" :ok)
    (#P"Drugs.aiml" :ok)
    (#P"Emotion.aiml" :ok)
    (#P"Food.aiml" :ok)
    (#P"Geography.aiml" :ok)
    (#P"Gossip.aiml" :ok)
    (#P"Happy.aiml" :ok)
    (#P"History.aiml" :ok)
    (#P"Human.aiml" :ok)
    (#P"Humor.aiml" :ok)
    (#P"IU.aiml" :ok)
    (#P"Inquiry.aiml" :ok)
    (#P"Integer.aiml" :ok)
    (#P"Interjection.aiml" :ok)
    (#P"Knowledge.aiml" :ok)
    (#P"Literature.aiml" :ok)
    (#P"Luckyslots.aiml" :ok)
    (#P"Money.aiml" :ok)
    (#P"Movies.aiml" :ok)
    (#P"Multiple.aiml" :ok)
    (#P"Music.aiml" :ok)
    (#P"Parts.aiml" :ok)
    (#P"Personality.aiml" :ok)
    (#P"Philosophy.aiml" :ok)
    (#P"Pickup.aiml" :ok)
    (#P"Politics.aiml" :ok)
    (#P"Predicates.aiml" :ok)
    (#P"Psychology.aiml" :ok)
    (#P"Reduce.aiml" :ok)
    (#P"Reducer.aiml" :ok)
    (#P"Reductions.aiml" :ok)
    (#P"Religion.aiml" :ok)
    (#P"Salutations.aiml" :ok)
    (#P"Science.aiml" :ok)
    (#P"Sex.aiml" :ok)
    (#P"Spam.aiml" :ok)
    (#P"Sports.aiml" :ok)
    (#P"Stack.aiml" :ok)
    (#P"Stories.aiml" :ok)
    (#P"That.aiml" :ok)
    (#P"Utilities.aiml" :ok)
    (#P"Wallace.aiml" :ok)
    (#P"Wordplay.aiml" :ok)
    (#P"Xfind.aiml" :ok)
    (#P"update.aiml" :ok)))

(defun pick-files (picker)
  (loop with fn = (compile-file-picker picker)
     for (file . attributes) in *files*
     when (funcall fn attributes) collect (cons file attributes)))

(defun compile-file-picker (thing)
  (cond
    ((eql thing t) (constantly t))
    ((eql thing nil) (lambda (attributes) (null attributes)))
    ((keywordp thing) (file-picker-symbol thing))
    ((consp thing)
     (case (first thing)
       (not (file-picker-not (second thing)))
       (and (file-picker-and (rest thing)))
       (or (file-picker-or (rest thing)))))))

(defun file-picker-symbol (symbol)
  (lambda (attributes) (find symbol attributes)))

(defun file-picker-not (symbol)
  (lambda (attributes) (not (find symbol attributes))))

(defun file-picker-and (clauses)
  (let ((clause-thunks (mapcar #'compile-file-picker clauses)))
    (lambda (attributes)
      (every #'(lambda (fn) (funcall fn attributes)) clause-thunks))))

(defun file-picker-or (clauses)
  (let ((clause-thunks (mapcar #'compile-file-picker clauses)))
    (lambda (attributes)
      (some #'(lambda (fn) (funcall fn attributes)) clause-thunks))))

(defun blank-string-p (x)
  (and (stringp x) (string= (string-trim *blanks* x) "")))

(defun blank-line-p (x)
  (and (stringp x) (string= (string #\Newline) x)))

(defun convert-aiml (files &key output-file)
  (if output-file
      (with-output-to-file (out output-file)
	(if (listp files)
	    (loop for (file . attributes) in files do
		 (format t "~&Converting ~a ~a" (enough-namestring file) attributes)
		 (format out "~&;; From ~a" (enough-namestring file))
		 (save-categories-to file out))
	    (save-categories-to files out)))
      (if (listp files)
	  (loop for (file . attributes) in files do
	       (format t "~&Converting ~a ~a" (enough-namestring file) attributes)
	       (save-categories file))
	  (save-categories files))))

(defun xml->xmls (file)
  (cxml:parse-file file (cxml-xmls:make-xmls-builder)))

(defun save-categories (input-file)
  (with-output-to-file (out (make-pathname :type "sexp" :defaults input-file))
    (save-categories-to input-file out)))

(defun save-categories-to (input-file out)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
	  (*package* #.*package*))
      (format out "~{~&~s~}" (grok-sexp (xml->xmls input-file))))))

(defun grok-sexp (sexp)
  (typecase sexp
    (string sexp)
    (cons
     (destructuring-bind (tag attributes . children) sexp
       (grok
	(intern (string-upcase tag) :keyword)
	attributes
	children)))))

(defun grok-template (template)
  (let ((template (mapcar #'grok-sexp (trim-template template))))
    (if (not (rest template)) (first template) (remove-if #'blank-line-p (trim-template template)))))

(defun trim-template (template)
  "Trim leading and trailing blank strings from a grokked template
unless it's a one item list of a blank string."
  (flet ((trim-list-front (list)
	   (member-if-not #'blank-string-p list))
	 (trim-list-back (list)
	   (let ((pos (position-if-not #'blank-string-p list :from-end t)))
	     (if pos
		 (ldiff list (nthcdr (1+ pos) list))
		 list)))
	 (trim-first (list)
	   (if (stringp (first list))
	       (cons (string-left-trim *blanks* (first list)) (rest list))
	       list))
	 (trim-last (list)
	   (let ((reversed (reverse list)))
	   (if (stringp (first reversed))
	       (nreverse (cons (string-right-trim *blanks* (first reversed)) (rest reversed)))
	       list))))
    (cond
      ((rest template)
       (trim-last (trim-first (trim-list-back (trim-list-front template)))))
      ((stringp (first template))
       (list (string-trim *blanks* (first template))))
      (t template))))

(defgeneric grok (tag attributes children))

(defmethod grok ((tag (eql :aiml)) attributes children)
  (loop for category in (remove-if #'stringp children) nconc (grok-sexp category)))

(defmethod grok ((tag (eql :topic)) attributes children)
  (destructuring-bind ((name value)) attributes
    (assert (string-equal name "name"))
    (loop for category in (remove-if #'stringp children) 
       for grokked = (grok-sexp category)
       when grokked collect (append (first grokked) (list :topic value)))))

(defmethod grok ((tag (eql :meta)) attributes children) nil)

(defmethod grok ((tag (eql :category)) (attributes null) children)
  (list (loop for part in (remove-if #'stringp children) nconc (grok-sexp part))))

(defmethod grok ((tag (eql :pattern)) (attributes null) children)
  (list (format nil "~{~a~}" children)))

(defmethod grok ((tag (eql :that)) (attributes null) children)
  (list :that (format nil "~{~a~}" children)))

(defmethod grok ((tag (eql :template)) (attributes null) children)
  (list :template (grok-template children)))

(defmethod grok ((tag (eql :random)) (attributes null) children)
  (let ((items (remove-if #'stringp children)))
    `(random
      ,@(loop for item in items collect
	     (destructuring-bind (tag attributes &rest template) item
	       (assert (string-equal tag "li"))
	       (assert (null attributes))
	       (grok-template template))))))

(defmethod grok ((tag (eql :condition)) attributes children)
  (ecase (length attributes)
    (0 (grok-multi-predicate-condition attributes children))
    (1 (grok-single-predicate-condition attributes children))
    (2 (grok-block-condition attributes children))))

(defun grok-multi-predicate-condition (attributes children)
  (assert (null attributes))
  `(cond
     ,@(loop for child in (remove-if #'stringp children) collect
	    (destructuring-bind (tag attributes &rest template) child
	      (assert (string-equal tag "li"))
	      (let ((template (grok-template template)))
		(if attributes
		    (let ((name (intern (string-upcase (cadr (assoc "name" attributes :test #'string-equal))) #.*package*))
			  (pattern (cadr (assoc "value" attributes :test #'string-equal))))
		      `((match ,name ,pattern) ,template))
		    `(t ,template)))))))

(defun grok-single-predicate-condition (attributes children)
  (destructuring-bind ((name variable)) attributes
    (assert (string-equal name "name"))
    `(case-match ,(intern (string-upcase variable) #.*package*)
		 ,@(loop for child in (remove-if #'stringp children) collect
			(destructuring-bind (tag attributes &rest template) child
			  (assert (string-equal tag "li"))
			  (destructuring-bind ((name pattern)) (or attributes (list (list "value" "*")))
			    (assert (string-equal name "value"))
			    (list pattern (grok-template template))))))))

(defun grok-block-condition (attributes children)
  (let ((pattern (cadr (assoc "value" attributes :test #'string-equal)))
	(variable (intern (string-upcase (cadr (assoc "name" attributes :test #'string-equal))) #.*package*)))
    `(when (match ,variable ,pattern) ,@(mapcar #'grok-sexp children))))

(defmethod grok ((tag (eql :srai)) (attributes null) children)
  `(process ,@(mapcar #'grok-sexp children)))

(defmethod grok ((tag (eql :bot)) attributes (children null))
  (destructuring-bind ((name value)) attributes
    (assert (string-equal name "name"))
    `(bot ,(intern (string-upcase value) #.*package*))))

(defmethod grok ((tag (eql :get)) attributes (children null))
  (destructuring-bind ((name value)) attributes
    (assert (string= name "name"))
    `(get ,(intern (string-upcase value) #.*package*))))

(defmethod grok ((tag (eql :set)) attributes children)
  (destructuring-bind ((name value)) attributes
    (assert (string= name "name"))
    `(set ,(intern (string-upcase value) #.*package*) ,(or (grok-template children) ""))))

(defmethod grok ((tag (eql :think)) (attributes null) children)
  `(think ,@(mapcar #'grok-sexp children)))

(defmethod grok ((tag (eql :person)) (attributes null) (children null))
  '(person (star)))

(defmethod grok ((tag (eql :person)) (attributes null) children)
  `(person ,@(mapcar #'grok-sexp children)))

(defmethod grok ((tag (eql :person2)) (attributes null) (children null))
  '(person2 (star)))

(defmethod grok ((tag (eql :person2)) (attributes null) children)
  `(person2 ,@(mapcar #'grok-sexp children)))

(defmethod grok ((tag (eql :gender)) (attributes null) (children null))
  '(gender (star)))

(defmethod grok ((tag (eql :gender)) (attributes null) children)
  `(gender ,@(mapcar #'grok-sexp children)))

(defmethod grok ((tag (eql :uppercase)) (attributes null) children)
  `(uppercase ,(grok-template children)))

(defmethod grok ((tag (eql :lowercase)) (attributes null) children)
  `(lowercase ,(grok-template children)))

(defmethod grok ((tag (eql :formal)) (attributes null) children)
  `(capitalize ,(grok-template children)))

(defmethod grok ((tag (eql :sentence)) (attributes null) children)
  `(sentence ,(grok-template children)))

(defmethod grok ((tag (eql :size)) (attributes null) (children null))
  `(size))

(defmethod grok ((tag (eql :version)) (attributes null) (children null))
  `(version))



(defmethod grok ((tag (eql :input)) attributes (children null))
  (if attributes
      (destructuring-bind ((name value)) attributes
	(assert (string= name "index"))
	(multiple-value-bind (index comma) (parse-integer value :junk-allowed t)
	  (if (< (1+ comma) (length value))
	      `(input ,index ,(parse-integer value :start (1+ comma)))
	      `(input ,index))))
      `(input)))

(defmethod grok ((tag (eql :star)) attributes (children null))
  (if attributes
      (destructuring-bind ((name value)) attributes
	(assert (string-equal name "index"))
	`(star ,(parse-integer value)))
      `(star)))

(defmethod grok ((tag (eql :thatstar)) attributes (children null))
  (if attributes
      (destructuring-bind ((name value)) attributes
	(assert (string-equal name "index"))
	`(thatstar ,(parse-integer value)))
      `(thatstar)))

(defmethod grok ((tag (eql :sr)) (attributes null) (children null))
  '(process (star)))

(defmethod grok ((tag (eql :that)) attributes children)
  (assert (null (remove-if #'blank-string-p children)))
  (destructuring-bind ((name value)) attributes
    (assert (string= name "index"))
    (multiple-value-bind (index comma) (parse-integer value :junk-allowed t)
      (if (< (1+ comma) (length value))
	  `(that ,index ,(parse-integer value :start (1+ comma)))
	  `(that ,index)))))

(defmethod grok ((tag (eql :that)) (attributes null) (children null))
  '(that))

(defmethod grok ((tag (eql :br)) (attributes null) (children null))
  " ")

(defmethod grok ((tag (eql :p)) (attributes null) (children null))
  " ")

(defmethod grok ((tag (eql :a)) attributes children)
  (format nil "~{~a~}" children))

;; This is a bit of a kludge but :ul is used in only once place in the ALICE AIML set. 
(defmethod grok ((tag (eql :ul)) (attributes null) children)
  (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~^~#[~;, and ~:;, ~]~}~]~}" 
	  (loop for item in (remove-if #'stringp children) collect
	       (destructuring-bind (tag attributes &rest children) item
		 (assert (string-equal tag "li"))
		 (assert (null attributes))
		 (grok-template children)))))

(defmethod grok ((tag (eql :em)) attributes children)
  (format nil "~{~a~}" children))

(defmethod grok ((tag (eql :b)) attributes children)
  (format nil "~{~a~}" children))

(defmethod grok ((tag (eql :img)) attributes children)
  "[IMAGE NOT AVAILABLE]")

(defmethod grok ((tag (eql :id)) (attributes null) (children null))
  '(id))

(defmethod grok ((tag (eql :date)) (attributes null) (children null))
  '(date))

(defmethod grok ((tag (eql :date)) attributes (children null))
  (destructuring-bind ((name value)) attributes
    (assert (string= name "format"))
    `(date ,value)))

(defmethod grok (tag attributes children)
  (format t "~&Generic grokking of tag: ~a; attributes: ~a; children: ~a" tag attributes children)
  `(,tag ,attributes ,@children))

#+(or)(defun parse-aiml (file)
  (klacks:with-open-source (s (cxml:make-source file))
    (loop for key = (klacks:peek s)
       while key do
	 (case key
	   (:start-element
	    (format t "~A {" (klacks:current-qname s)))
	   (:end-element
	    (format t "}")))
	 (klacks:consume s))))

(defun parse-aiml (file)
  (klacks:with-open-source (s (cxml:make-source file))
    (parse-category s)))


(defun parse-category (s)
  (klacks:find-element s "category")
  (klacks:consume s)
  (let ((pattern (parse-pattern s))
	(template (parse-template s)))
    (format t "~&pattern: ~a~&template: ~a~2%" pattern template)))
    


(defun parse-pattern (s)
  (klacks:find-element s "pattern")
  (klacks:consume s)
  (format nil "~{~a~}"
	  (loop for key = (klacks:peek s)
	     while (eql key :characters)
	     collect (nth-value 1 (klacks:consume s)))))

(defun parse-template (s)
  (klacks:find-element s "template")
  (klacks:consume s)
  (loop for key = (klacks:peek s)
     while (and (not (eql key :end-element))
		(string/= (nth-value 1 (klacks:peek s)) "template"))
     collect (nth-value 1 (klacks:consume s))))
  
  


;;; These functions are for checking whether the rules parsed from
;;; AIML are well formed.

(defun check-rules (rules)
  "Compile a list of (pattern &key that topic template) lists into a list of
patterns that can be passed to compile-patterns."
  (let ((good ())
	(bad ()))
    (loop for rule in rules do
	 (handler-case
	     (destructuring-bind (pattern &key that topic template) rule
	       (declare (ignore pattern that topic))
	       (compile-template template)
	       (push rule good))
	   (error ()
	     (push rule bad))))
    (values
     (nreverse good)
     (nreverse bad))))

(defun split-rules-file (file)
  (flet ((save (rules file)
	   (with-output-to-file (out file)
	     (with-standard-io-syntax 
	       (let ((*print-case* :downcase)
		     (*package* #.*package*))
		 (format out "~{~&~s~}" rules))))))
  (multiple-value-bind (good bad) (check-rules (file->list file))
    (save good "good.sexp")
    (save bad "bad.sexp")
    (values (length good) (length bad)))))
