(in-package :com.gigamonkeys.bot)

(defparameter *verbose* nil)

(defclass bot ()
  ((brain :initarg :brain :accessor brain)
   (conversations :initform (make-hash-table :test #'equal) :accessor conversations))
  (:documentation "A bot is an entity with a single brain and capable
  of having one or more conversations."))

(defclass brain ()
  ((rules :initform nil :initarg :rules :accessor rules)
   (matcher :initarg :matcher :accessor matcher))
  (:documentation "A bot's brain consists of a list of rules which are
  compiled into a matcher function."))

(defclass file-brain (brain)
  ((file :initarg :file :accessor file)
   (new-rules-file :accessor new-rules-file))
  (:documentation "A brain that loads its rules from a file and can
  save new rules to a side file."))

(defclass conversation ()
  ((topic :initform nil :accessor topic)
   (thats :initform nil :accessor thats)
   (inputs :initform nil :accessor inputs)
   (bot-variables :initform (make-bot-variables) :accessor bot-variables)
   (variables :initform (make-hash-table) :accessor variables)
   (functions :initform (make-hash-table) :accessor functions)
   (bot :initarg :bot :accessor bot))
  (:documentation "A conversation tracks the state of the interactions
  with a bot. A single bot can maintain multiple conversations, each
  with their own state."))

(defmethod initialize-instance :after ((brain brain) &key &allow-other-keys)
  (compile-matcher brain))

(defmethod initialize-instance :after ((brain file-brain) &key &allow-other-keys)
  (reload-brain brain))

(defun find-conversation (bot speaker)
  (or (gethash speaker (conversations bot))
      (setf (gethash speaker (conversations bot)) (make-conversation bot))))

(defgeneric make-conversation (bot))

;; FIXME 2010-02-05 <peter@greyhound> -- this needs to be generalized.
;; Or if we change how we compile templates to not need the FUNCTIONS
;; hash, then this just makes an instance of CONVERSATION.
(defmethod make-conversation ((bot bot))
  (let ((c (make-instance 'conversation :bot bot)))
    (add-function c 'common-lisp-symbol-p)
    (add-function c 'common-lisp-function-p)
    c))

(defun add-function (conversation name &optional function)
  (setf (gethash name (functions conversation)) (or function name)))

(defun new-rules-file-name (file)
  (make-pathname :name (format nil "~a-new-rules" (pathname-name file)) :defaults file))

(defun in-exchange-p (conversation)
  (string-equal (gethash 'in-exchange-p (variables conversation)) "true"))

(defun add-rules-from-file (brain file)
  (with-slots (rules) brain
    (loop for rule in (file->list file) do (push rule rules))))

(defun reload-brain (brain)
  (with-slots (file new-rules-file rules) brain
    (setf new-rules-file (new-rules-file-name file))
    (add-rules-from-file brain file)
    (add-rules-from-file brain new-rules-file)
    (compile-matcher brain)))

(defun compile-matcher (brain)
  (with-slots (rules matcher) brain
    (setf matcher (compile-patterns (compile-rules rules)))))

(defun add-rule (conversation pattern template topic that)
  (learn (brain (bot conversation))
         `(,pattern
           :template ,template
           ,@(if topic (list :topic topic))
           ,@(if that (list :that that)))))

(defgeneric learn (brain rule))

(defmethod learn ((brain brain) rule)
  (with-slots (rules) brain
    (push rule rules)
    (compile-matcher brain)))

(defmethod learn :before ((brain file-brain) rule)
  (with-open-file (out (new-rules-file brain) :direction :output :if-exists :append :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-case* :downcase)
            (*package* #.*package*))
        (print rule out)))))



;;; Conversational state

(defun make-bot-variables ()
  (let ((h (make-hash-table)))
    (setf (gethash 'master h) "Peter Seibel")
    (setf (gethash 'botmaster h) "author")
    (setf (gethash 'birthday h) "February 1st, 2010")
    h))

(defun common-lisp-symbol-p (string)
  (find-symbol (string-upcase string) :common-lisp))

(defun common-lisp-function-p (string)
  (and (common-lisp-symbol-p string)
       (fboundp (intern (string-upcase string) :cl))))

(defun conversational-response (conversation input responder)
  (push input (inputs conversation))
  (loop for sentence in (split-sentences (normalize-input input))  do
       (let ((response (process conversation sentence)))
         (cond
           (response
            (push response (thats conversation))
            (funcall responder response))
           (t (funcall responder "Sorry, I don't understand."))))))

(defun quit-conversation (goodbye)
  (throw 'quit-conversation goodbye))

(defun get-response (&optional (prompt "> "))
  (format *standard-output* "~&~a" prompt)
  (finish-output *standard-output*)
  (read-line *standard-input* nil nil))

(defun process (conversation sentence)
  "Process a sentence and return the response."
  (with-slots (topic variables bot) conversation
    (with-slots (matcher) (brain bot)
      (let ((input-path (make-input-path sentence conversation)))
        (multiple-value-bind (match rule stars) (funcall matcher input-path)
          (and match (funcall rule conversation stars)))))))

(defun make-input-path (sentence conversation)
  (with-slots (thats topic) conversation
    (make-match-path
     (copy-list sentence)
     (if thats (first (last (split-sentences (normalize-input (first thats))))) nil)
     (copy-list topic))))

(defun make-match-path (pattern that topic)
  (nconc
   pattern
   (cons 'that (or that (list "*")))
   (cons 'topic (or topic (list "*")))))

;;; Matchers -- compile the list of patterns down to a tree of
;;; closures that match a given input and return the compiled template
;;; function so it can be invoked.

(defun make-wildcard-matcher (continuation)
  (lambda (input)
    (loop for tail in (cons nil (nreverse (maplist #'identity input))) do
         (when *verbose* (format t "~&wildcard matcher: continuing with ~s" tail))
         (multiple-value-bind (match value wildcards) (funcall continuation tail)
           (when match
             (let ((matched-tokens (ldiff input tail)))
               (when *verbose*
                 (format t "~&Matched wildcard with matched-tokens: ~s" matched-tokens))
               (return (values match value (cons matched-tokens wildcards)))))))))

(defun make-marker-matcher (marker continuation)
  (lambda (input)
    (setf input (member-if-not #'whitespace-or-special-token-p input))
    (when (eql marker (first input))
      (when *verbose* (format t "~&Matched marker: ~a" marker))
      (multiple-value-bind (match value wildcards) (funcall continuation (rest input))
        (when *verbose* "After marker new wildcards: ~s" wildcards)
        (when match
          (values match value (cons marker wildcards)))))))

(defun make-token-matcher (token continuation)
  (cond
    ((contraction-p token)
     (make-contraction-matcher token continuation))
    ((normal-token-p token)
     (make-normal-token-matcher token continuation))
    ((special-token-p token)
     (make-special-token-matcher token continuation))
    (t (error "Bad token: ~s" token))))

(defun make-normal-token-matcher (token continuation)
  "Match a given token, skipping any whitspace and special punctuation
tokens."
  (lambda (input)
    (when *verbose* (format t "~&normal-token-matcher input: ~s" input))
    (setf input (member-if-not #'whitespace-or-special-token-p input))
    (let ((next (first input)))
      (when (and (stringp next) (string-equal token next))
        (when *verbose* (format t "~&Matched token: ~a with rest: ~a" token (rest input)))
        (funcall continuation (rest input))))))

(defun make-special-token-matcher (token continuation)
  "For explicitly matching special punctuation tokens, skipping
whitespace tokens."
  (lambda (input)
    (setf input (member-if-not #'whitespace-token-p input))
    (let ((next (first input)))
      (when (and (stringp next) (string-equal token next))
        (when *verbose* (format t "~&Matched special token: ~a with rest: ~a" token (rest input)))
        (funcall continuation (rest input))))))

(defun make-end-matcher (value)
  (lambda (input)
    (when *verbose* (format t "~&end-matcher input: ~s" input))
    (when (null (member-if-not #'whitespace-or-special-token-p input))
      (when *verbose* (format t "~&Matched end."))
      (values t value))))

(defun make-or-matcher (matchers)
  (lambda (input)
    (loop for m in matchers do
         (multiple-value-bind (match value wildcards) (funcall m input)
           (if match (return (values match value wildcards)))))))

(defmacro multiple-value-or (&body forms)
  (if forms
      (with-gensyms (results)
        `(let ((,results (multiple-value-list ,(first forms))))
           (if (first ,results)
               (values-list ,results)
               (multiple-value-or ,@(rest forms)))))
      nil))

(defun make-contraction-matcher (token continuation)
  "Make a matcher that will match a contraction as a single token or
the expansion of the contraction as multiple takens. So if the token
is 'isn't', (with the expansion ('is' 'not') this matcher will match
if the next tokens on the input are either 'isn't' or 'is' followed by
'not'. Thus patterns should be written with contractions to be
maximally general."
  (flet ((msm (expansion) (make-sequence-matcher expansion continuation)))
    (let ((token-matcher (make-normal-token-matcher token continuation))
          (expansion-matchers (mapcar #'msm (contraction-expansions token))))
      (make-or-matcher (list* token-matcher expansion-matchers)))))

(defun make-sequence-matcher (sequence continuation)
  "Make a matcher that will match a sequence of tokens."
  (loop for token in (reverse sequence)
     for k = continuation then matcher
     for matcher = (make-token-matcher token k)
     finally (return matcher)))

(defun compile-rules (rules)
  "Compile a list of (pattern &key that topic template) lists into a list of
patterns that can be passed to compile-patterns."
  (compile-normalized-rules (normalize-rules rules)))

(defun normalize-rules (rules)
  "Normalizes the patterns in the rules and converts them into a
simple list (with no keywords)."
  (loop for rule in rules collect
       (destructuring-bind (pattern &key that topic template) rule
         (list
          (normalize-pattern pattern)
          (and that (normalize-pattern that))
          (and topic (normalize-pattern topic))
          template))))

(defun compile-normalized-rules (rules)
  "Compile a list of (pattern that topic template) lists into a list
of patterns that can be passed to compile-patterns. Each pattern is an
improper list with the final CDR being the compiled template."
  (loop for rule in rules collect
       (destructuring-bind (pattern that topic template) rule
         (nconc
          (make-match-path pattern that topic)
          (let ((compiled-template (compile-template template)))
            (lambda (conversation stars)
              (when *verbose* (format t "~&Matched pattern: ~s~&That: ~s~&Topic: ~s~&Stars: ~s" pattern that topic stars))
              (funcall compiled-template conversation stars)))))))

(defun compile-rules-file (file)
  (compile-patterns (compile-rules (file->list file))))

(defun compile-patterns (patterns)
  (unless (null patterns)
    (compile-trees (rules->trees (sort (copy-list patterns) #'pattern<)))))

(defun compile-trees (trees)
  (if (rest trees)
      (make-or-matcher (mapcar #'compile-tree trees))
      (compile-tree (first trees))))

(defun compile-tree (tree)
  (flet ((value-p (x) (atom x))
         (wildcard-p (x) (or (string-equal (car x) "_") (string-equal (car x) "*")))
         (marker-p (x) (or (eql (car x) 'that) (eql (car x) 'topic))))
    (cond
      ((value-p tree) (make-end-matcher tree))
      ((wildcard-p tree) (make-wildcard-matcher (compile-trees (cdr tree))))
      ((marker-p tree) (make-marker-matcher (car tree) (compile-trees (cdr tree))))
      (t (make-token-matcher (car tree) (compile-trees (cdr tree)))))))

(defun sort-brain (file)
  "Sort a brain file into a canonical order, writing it out to a new file."
  (with-output-to-file (out (make-pathname :name (format nil "sorted-~a" (pathname-name file)) :defaults file))
    (with-standard-io-syntax
      (let ((*print-case* :downcase)
            (*package* #.*package*))
        (format out "~{~&~s~}" (mapcar #'(lambda (x)
                                           (setf (first x) (string-downcase (first x)))
                                           x) (sort (file->list file) #'rule<)))))))

(defun extract-templates (file)
  (mapcar #'(lambda (x) (getf (rest x) :template))  (file->list file)))

(defun rule< (rule-1 rule-2)
  (destructuring-bind (pattern-1 &key ((:topic topic-1) "*") ((:that that-1) "*") ((:template template-1))) rule-1
    (destructuring-bind (pattern-2 &key ((:topic topic-2) "*") ((:that that-2) "*") ((:template template-2))) rule-2
      (setf pattern-1 (normalize-pattern pattern-1))
      (setf pattern-2 (normalize-pattern pattern-2))
      (setf topic-1 (normalize-pattern topic-1))
      (setf topic-2 (normalize-pattern topic-2))
      (setf that-1 (normalize-pattern that-1))
      (setf that-2 (normalize-pattern that-2))

      (cond
        ((pattern< topic-1 topic-2) t)
        ((pattern< topic-2 topic-1) nil)

        ((pattern< that-1 that-2) t)
        ((pattern< that-2 that-1) nil)

        ((template< template-1 template-2) t)
        ((template< template-2 template-1) nil)

        ((pattern< pattern-1 pattern-2) t)
        ((pattern< pattern-2 pattern-1) nil)

        (t (error "Identical rules: ~s and ~s" rule-1 rule-2))))))

(defun template< (template-1 template-2)
  (flet ((process-star-p (template)
           (equal template '(process (star))))
         (process-string-p (template)
           (and (consp template)
                (eql (car template) 'process)
                (stringp (cadr template))
                (null (cddr template))))
         (process-something-p (template)
           (and (consp template) (eql (car template) 'process))))



    (cond
      ((and (process-star-p template-1) (not (process-star-p template-2))) t)
      ((and (process-star-p template-2) (not (process-star-p template-1))) nil)

      ((and (process-string-p template-1) (not (process-string-p template-2))) t)
      ((and (process-string-p template-2) (not (process-string-p template-1))) nil)

      ((and (process-something-p template-1) (not (process-something-p template-2))) t)
      ((and (process-something-p template-2) (not (process-something-p template-1))) nil)

      (t nil))))

(defun pattern< (pattern-1 pattern-2)
  (cond
    ;; Hit the end of both patterns at the same time so p1 = p2 => not
    ;; p1 < p2
    ((and (atom pattern-1) (atom pattern-2)) nil)

    ;; Hit end of first pattern, and we know we're not at end of
    ;; second so not p2 < p1 => not p1 < p2
    ((atom pattern-1) nil)

    ;; Hit end of second pattern, and we know we're not at end of
    ;; first, so p1 < p2
    ((atom pattern-2) t)
    (t
     (let ((token-1 (car pattern-1))
           (token-2 (car pattern-2)))
       (cond
         ((token< token-1 token-2) t)
         ((token< token-2 token-1) nil)
         (t (pattern< (cdr pattern-1) (cdr pattern-2))))))))

(defun token< (token-1 token-2)
  (cond
    ((string-equal token-1 "_") (not (not (string-not-equal token-2 "_"))))
    ((string-equal token-2 "_") nil)
    ((string-equal token-1 "*") nil)
    ((string-equal token-2 "*") t)
    (t (not (not (string-lessp token-1 token-2))))))

(defun rules->trees (rules)
  "Take a list of flat rules and return a list of one or more trees"
  (unless (null rules)
    (cond
      ((atom (car rules))
       (cons (car rules) (rules->trees (cdr rules))))
      (t
       (let* ((start (caar rules))
              (rest (rest rules))
              (next (or (position start rest :test (complement #'eql) :key #'car) (length rest)))
              (remaining (nthcdr next rest))
              (group (ldiff rules remaining)))
         (cons
          (cons start (rules->trees (mapcar #'cdr group)))
          (rules->trees remaining)))))))

(defun list-to-tree (list)
  (cond
    ((atom list) list)
    (t (cons (first list) (list (list-to-tree (rest list)))))))

(defun compile-simple-pattern (pattern)
  (compile-tree (list-to-tree (normalize-pattern pattern))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling templates.

(defparameter *null-template* (constantly ""))

(defun compile-template (template)
  "Compile a list of template elements into a function that takes the
conversation and list of star matches and returns a string of text."
  (cond
    ((null template) *null-template*)
    ((or (not (consp template))
         (and (consp template) (symbolp (car template))))
     (compile-template-element template *null-template*))
    (t
     (compile-template-element
      (first template)
      (compile-template (rest template))))))

(defgeneric compile-template-element (element continuation))

(defmethod compile-template-element ((element string) continuation)
  (compile-literal element continuation))

(defmethod compile-template-element ((element number) continuation)
  (compile-literal (princ-to-string element) continuation))

(defmethod compile-template-element ((element cons) continuation)
  (compile-operator (first element) (rest element) continuation))

(defun compile-literal (literal continuation)
  (lambda (conversation stars)
    (concatenate 'string
                 literal
                 (funcall continuation conversation stars))))


(defgeneric compile-operator (operator arguments continuation))

;;; Recursive operators

(defmethod compile-operator ((operator (eql 'process)) args continuation)
  (let ((args-thunk (compile-template args)))
    (lambda (conversation stars)
      (concatenate
       'string
       (process conversation (normalize-input (funcall args-thunk conversation stars)))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'think)) args continuation)
  (let ((args-thunk (compile-template args)))
    (lambda (conversation stars)
      (process conversation (normalize-input (funcall args-thunk conversation stars)))
      (funcall continuation conversation stars))))

;;; Capture

(defmethod compile-operator ((operator (eql 'star)) args continuation)
  (destructuring-bind (&optional (idx 0)) args
    (lambda (conversation stars)
      (let ((pattern-stars (ldiff stars (member 'that stars))))
        (concatenate
         'string
         (format nil "~{~a~}" (trim-whitespace-tokens (nth idx pattern-stars)))
         (funcall continuation conversation stars))))))

(defmethod compile-operator ((operator (eql 'thatstar)) args continuation)
  (destructuring-bind (&optional (idx 0)) args
    (lambda (conversation stars)
      (let ((that-stars (ldiff (cdr (member 'that stars)) (member 'topic stars))))
        (concatenate
         'string
         (format nil "~(~{~a~^ ~}~)" (trim-whitespace-tokens (nth idx that-stars)))
         (funcall continuation conversation stars))))))

(defmethod compile-operator ((operator (eql 'that)) args continuation)
  ;; FIXME 2010-02-01 <peter@greyhound> -- need to implement sentence-idx
  (destructuring-bind (&optional (idx 0) sentence-idx) args
    (declare (ignore sentence-idx))
    (lambda (conversation stars)
      (concatenate
       'string
       (or (nth idx (thats conversation)) "")
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'input)) args continuation)
  (destructuring-bind (&optional (idx 0)) args
    (lambda (conversation stars)
      (concatenate
       'string
       (or (nth idx (inputs conversation)) "")
       (funcall continuation conversation stars)))))

;;; Variables.

(defmethod compile-operator ((operator (eql 'get)) args continuation)
  (destructuring-bind (name) args
    (lambda (conversation stars)
      (when *verbose*
        (format t "~&Getting ~a from ~a" name (variables conversation)))
      (concatenate
       'string
       (princ-to-string (gethash name (variables conversation)))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'set)) args continuation)
  (destructuring-bind (name value-template) args
    (let ((value-thunk (compile-template value-template)))
      (lambda (conversation stars)
        (setf (gethash name (variables conversation)) (funcall value-thunk conversation stars))
        (when *verbose*
          (format t "~&Setting ~a in ~a" name (variables conversation)))
        (concatenate
         'string
         ;(if (stringp value-template) (string-upcase value-template)
             (gethash name (variables conversation))
             ;)
         (funcall continuation conversation stars))))))

(defmethod compile-operator ((operator (eql 'silent-set)) args continuation)
  (destructuring-bind (name value-template) args
    (let ((value-thunk (compile-template value-template)))
      (lambda (conversation stars)
        (setf (gethash name (variables conversation)) (funcall value-thunk conversation stars))
        (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'bot)) args continuation)
  (destructuring-bind (name) args
    (lambda (conversation stars)
      (concatenate
       'string
       (princ-to-string (gethash name (bot-variables conversation)))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'get-topic)) args continuation)
  (lambda (conversation stars)
    (concatenate
     'string
     (princ-to-string (topic conversation))
     (funcall continuation conversation stars))))

(defmethod compile-operator ((operator (eql 'set-topic)) args continuation)
  (let ((value-thunk (compile-template args)))
    (lambda (conversation stars)
      (setf (topic conversation) (normalize-input (funcall value-thunk conversation stars)))
      (when *verbose*
        (format t "Set topic to: ~s" (topic conversation)))
      (funcall continuation conversation stars))))


(defmethod compile-operator ((operator (eql 'clear-topic)) args continuation)
  (lambda (conversation stars)
    (setf (topic conversation) nil)
    (when *verbose*
      (format t "Cleared topic."))
    (funcall continuation conversation stars)))

(defmethod compile-operator ((operator (eql 'add-rule)) args continuation)
  (destructuring-bind (pattern template &key topic that) args
    (let ((pattern-thunk (compile-template pattern))
          (template-thunk (compile-template template))
          (topic-thunk (and topic (compile-template topic)))
          (that-thunk (and that (compile-template that))))
      (lambda  (conversation stars)
        (add-rule conversation
                  (funcall pattern-thunk conversation stars)
                  (funcall template-thunk conversation stars)
                  (and topic-thunk (funcall topic-thunk conversation stars))
                  (and that-thunk (funcall that-thunk conversation stars)))
        (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'function)) args continuation)
    (let ((name (first args))
          (arg-thunks (mapcar #'compile-value-thunk (rest args))))
      (lambda (conversation stars)
        (concatenate
         'string
         (apply (gethash name (functions conversation))
                (mapcar #'(lambda (x) (funcall x conversation stars)) arg-thunks))
         (funcall continuation conversation stars)))))

;;; Text formatting

(defmethod compile-operator ((operator (eql 'uppercase)) args continuation)
  (let ((value-thunk (compile-template args)))
    (lambda (conversation stars)
      (concatenate
       'string
       (string-upcase (funcall value-thunk conversation stars))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'lowercase)) args continuation)
  (let ((value-thunk (compile-template args)))
    (lambda (conversation stars)
      (concatenate
       'string
       (string-downcase (funcall value-thunk conversation stars))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'capitalize)) args continuation)
  (let ((value-thunk (compile-template args)))
    (lambda (conversation stars)
      (concatenate
       'string
       (string-capitalize (funcall value-thunk conversation stars))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'sentence)) args continuation)
  (let ((value-thunk (compile-template args)))
    (lambda (conversation stars)
      (let ((text (funcall value-thunk conversation stars)))
        (concatenate
         'string
         (string-upcase (string-downcase text) :end 1)
         (funcall continuation conversation stars))))))


(defmethod compile-operator ((operator (eql 'person)) args continuation)
  (let ((args-thunk (compile-template args))
        (substitutions '((:i . :you) (:me . :you) (:we . :you) (:us . :you) (:you . :me))))
    (lambda (conversation stars)
      (concatenate
       'string
       (format nil "~(~{~a~^ ~}~)" (sublis substitutions (normalize-input (funcall args-thunk conversation stars))))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'person2)) args continuation)
  (let ((args-thunk (compile-template args))
        (substitutions '((:i . :he) (:me . :him) (:we . :they) (:us . :them) (:he . :i) (:him . :me) (:she . :i) (:her . :me) (:they . :we) (:them . :us))))
    (lambda (conversation stars)
      (concatenate
       'string
       (format nil "~(~{~a~^ ~}~)" (sublis substitutions (normalize-input (funcall args-thunk conversation stars))))
       (funcall continuation conversation stars)))))

(defmethod compile-operator ((operator (eql 'gender)) args continuation)
  (let ((args-thunk (compile-template args))
        (substitutions '((:he . :she) (:she . :he) (:his . :hers) (:hers . :his) (:him . :her) (:her . :him))))
    (lambda (conversation stars)
      (concatenate
       'string
       (format nil "~(~{~a~^ ~}~)" (sublis substitutions (normalize-input (funcall args-thunk conversation stars))))
       (funcall continuation conversation stars)))))

;;; Choice

(defun compile-test-thunk (test)
  (typecase test
    (symbol (compile-variable-access test))
    (cons (compile-test-expression (first test) (rest test)))))

(defun compile-variable-access (name)
  (lambda (conversation stars)
    (declare (ignore stars))
    (case name
      (t t)
      (nil nil)
      (gethash name (variables conversation)))))

(defgeneric compile-test-expression (op args))

(defmethod compile-test-expression ((op (eql '=)) args)
  (destructuring-bind (variable value) args
    (let ((value-thunk (compile-template value)))
      (lambda (conversation stars)
        (string-equal
         (gethash variable (variables conversation))
         (funcall value-thunk conversation stars))))))

(defmethod compile-test-expression ((op (eql '!=)) args)
  (destructuring-bind (variable value) args
    (let ((value-thunk (compile-template value)))
      (lambda (conversation stars)
        (string-not-equal
         (gethash variable (variables conversation))
         (funcall value-thunk conversation stars))))))

(defmethod compile-test-expression ((op (eql 'match)) args)
  (destructuring-bind (value pattern) args
    (let ((value-thunk (compile-value-thunk value))
          (pattern-thunk (compile-simple-pattern pattern)))
      (lambda (conversation stars)
        (let ((value (funcall value-thunk conversation stars)))
          (funcall pattern-thunk (normalize-input value)))))))

(defmethod compile-test-expression (op args)
  (let ((arg-thunks (mapcar #'compile-value-thunk args)))
    (lambda (conversation stars)
      (apply (gethash op (functions conversation))
             (mapcar #'(lambda (x) (funcall x conversation stars)) arg-thunks)))))

(defun compile-value-thunk (value)
  (etypecase value
    (symbol (compile-variable-access value))
    (cons (compile-template value))))

(defmethod compile-operator ((operator (eql 'if)) args continuation)
  (destructuring-bind (test then &optional else) args
    (let ((test-thunk (compile-test-thunk test))
          (then-thunk (compile-template then))
          (else-thunk (compile-template else)))
      (lambda (conversation stars)
        (concatenate
         'string
         (funcall
          (if (funcall test-thunk conversation stars) then-thunk else-thunk)
          conversation stars)
         (funcall continuation conversation stars))))))

(defmethod compile-operator ((operator (eql 'when)) args continuation)
  (compile-template-element `(if ,(first args) ,(rest args) nil) continuation))

(defmethod compile-operator ((operator (eql 'unless)) args continuation)
  (compile-template-element `(if ,(first args) nil ,(rest args)) continuation))

(defmethod compile-operator ((operator (eql 'cond)) args continuation)
  (compile-template-element (cond->ifs args) continuation))

(defun cond->ifs (clauses)
  (when clauses
    (destructuring-bind ((first-test first-template) &rest remaining-clauses) clauses
      `(if ,first-test ,first-template ,(cond->ifs remaining-clauses)))))

(defmethod compile-operator ((operator (eql 'case-match)) args continuation)
  (destructuring-bind (variable &rest clauses) args
    (let ((value-thunk (compile-variable-access variable))
          (clause-thunks (mapcar #'compile-case-match-clause clauses)))
      (lambda (conversation stars)
        (concatenate
         'string
         (let ((value (funcall value-thunk conversation stars)))
           (or (some (lambda (x) (funcall x value conversation stars)) clause-thunks) "")))
        (funcall continuation conversation stars)))))

(defun compile-case-match-clause (clause)
  (destructuring-bind (pattern template) clause
    (let ((pattern-matcher (compile-simple-pattern pattern))
          (template-thunk (compile-template template)))
      (lambda (value conversation stars)
        (when (funcall pattern-matcher (normalize-input value))
          (funcall template-thunk conversation stars))))))

(defmethod compile-operator ((operator (eql 'random)) args continuation)
  (let ((n (length args))
        (value-thunks (mapcar #'compile-template args)))
    (lambda (conversation stars)
      (concatenate
       'string
       (funcall (nth (random n) value-thunks) conversation stars)
       (funcall continuation conversation stars)))))


;;; Misc

(defmethod compile-operator ((operator (eql 'id)) (args null) continuation)
  (lambda (conversation stars)
    (concatenate
     'string
     "ID NOT AVAILABLE"
     (funcall continuation conversation stars))))

(defmethod compile-operator ((operator (eql 'date)) args continuation)
  (lambda (conversation stars)
    (concatenate
     'string
     "DATE NOT AVAILABLE"
     (funcall continuation conversation stars))))

(defmethod compile-operator ((operator (eql 'size)) args continuation)
  (lambda (conversation stars)
    (concatenate
     'string
     "SIZE NOT AVAILABLE"
     (funcall continuation conversation stars))))

(defmethod compile-operator ((operator (eql 'version)) args continuation)
  (lambda (conversation stars)
    (concatenate
     'string
     "1.0"
     (funcall continuation conversation stars))))

;;; Quitting

(defmethod compile-operator ((operator (eql 'quit)) args continuation)
  (let ((args-thunk (compile-template args)))
    (lambda (conversation stars)
      (quit-conversation (funcall args-thunk conversation stars)))))

(defmethod compile-operator ((operator (eql 'dump)) args continuation)
  (lambda (conversation stars)
    (format t "~&Topic: ~s" (topic conversation))
    (format t "~&Thats: ~s" (thats conversation))
    (format t "~&Stars: ~s" stars)))


;;; REPL interaction with bot

(defun file-conversation (file)
  (have-conversation (make-instance 'bot :brain (make-instance 'file-brain :file file))))

(defun have-conversation (bot)
  (let ((conversation (make-instance 'conversation :bot bot)))
    (add-function conversation 'common-lisp-symbol-p)
    (add-function conversation 'common-lisp-function-p
                  (lambda (string)
                    (and (common-lisp-symbol-p string)
                         (fboundp (intern (string-upcase string) :cl)))))

    (add-rule conversation "QUIT" '(quit) nil nil)
    (add-rule conversation "Q" '(quit) nil nil)

    (catch 'quit-conversation
      (loop for input = (get-response) do
           (let ((responses ()))
             (conversational-response conversation input (lambda (response) (push response responses)))
             (format t "~&~{~a~^ ~}" (delete-duplicates (nreverse responses) :test #'string=)))))
    nil))
