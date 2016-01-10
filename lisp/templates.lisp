(in-package :com.gigamonkeys.bot.templates)

(defvar *template-output* nil)

(defvar *stars* nil)
(defvar *conversation* nil)

(defun compile-template (template)
  (compile nil `(lambda (*stars* *conversation*) (expand ,@(templatify template)))))

(defun templatify (x)
  (if (or (atom x) (symbolp (car x))) (list x) x))

(defun emit (thing)
  (when thing (princ thing *template-output*)))

(defmacro expand (&rest template)
  `(with-output-to-string (*template-output*)
     (progn ,@(loop for e in template collect `(emit ,e)))))

;;; Special functions and macros for use in templates

(defun star (&optional (idx 0))
  (nth idx (ldiff *stars* (member 'that *stars*))))

(defun that-star (&optional (idx 0))
  (nth idx (ldiff (cdr (member 'that *stars*)) (member 'topic *stars*))))

(defun topic-star (&optional (idx 0))
  (nth idx (cdr (member 'topic *stars*))))

(defun that (&optional (idx 0))
  ;; AIML also has a sentence index.
  (nth idx (thats *conversation*)))

(defun input (&optional (idx 0))
  ;; AIML also has a sentence index.
  (nth idx (inputs *conversation*)))

(defmacro get 

(defmacro random (&rest choices)
  (let ((n (length choices)))
    `(case (cl:random ,n)
     ,@(loop for i from 0 below n
	  for choice in choices 
	  collect `(,i (expand ,@(com.gigamonkeys.bot::templatify choice)))))))

(defmacro process (&rest template)
  `(com.gigamonkeys.bot::process
    *conversation*
    (com.gigamonkeys.bot::normalize-input (expand ,@template))))
  



