(in-package :com.gigamonkeys.bot)

(defparameter *whitespace-token* " ")

(defparameter *contractions*
  '(("let's" "let us")
    ("'s$" " is")
    ("n't$" " not")
    ("'re$" " are")
    ("'m$" " am")
    ("'ll$" " will")
    ("'ve" " have")
    ("'d" " would" " had")))

(defun normalize-input (text)
  (loop for token in (interleave (split "\\s+" (string-trim " " text)) *whitespace-token*) nconc (split-off-punctuation token #'alphanumericp)))

(defun normalize-pattern (text)
  (remove-if #'whitespace-token-p (normalize-input text)))

(defun normal-token-p (token)
  (and (stringp token) (alphanumericp (char token 0))))

(defun special-token-p (token)
  (and (stringp token)
       (string/= token *whitespace-token*)
       (not (alphanumericp (char token 0)))))

(defun whitespace-token-p (token)
  (and (stringp token)
       (string= token *whitespace-token*)))

(defun whitespace-or-special-token-p (token)
  (or (whitespace-token-p token)
      (special-token-p token)))

(defun contraction-p (token)
  (position #\' token))

(defun contraction-expansions (token)
  (let ((expansions 
	 (loop for (pattern . replacements) in *contractions* 
	    for scanner = (create-scanner pattern :case-insensitive-mode t)
	    when (scan scanner token) nconc 
	      (loop for r in replacements collecting (normalize-pattern (regex-replace scanner token r))))))
    (or expansions (error "Don't know how to expand ~s" token))))

(defun explode-string (string)
  (map 'list #'string string))

(defun interleave (list separator)
  (list* (first list) (mapcan #'(lambda (x) (list separator x)) (rest list))))

(defun split-off-punctuation (string predicate)
  "Split punctuation at the beginning and end of the token off into separate tokens."
  (cond
    ((= (length string) 1)
     (list string))
    ((notany predicate string)
     (explode-string string))
    (t
     (let* ((first-non-punctuation (position-if predicate string))
	    (last-non-punctuation (1+ (position-if predicate string :from-end t)))
	    (first (if (> first-non-punctuation 0) (subseq string 0 first-non-punctuation)))
	    (last (if (< last-non-punctuation (length string)) (subseq string last-non-punctuation (length string))))
	    (middle (subseq string first-non-punctuation last-non-punctuation)))
       
       (remove nil `(,@(explode-string first) ,middle ,@(explode-string last)))))))

(defun split-sentences (normalized-input)
  (let ((sentences ())
	(current-sentence ()))
    (loop for x in normalized-input
       when (and (member x '("." "!" "?") :test #'string=) current-sentence)
       do
	 (push x current-sentence)
	 (push (nreverse current-sentence) sentences)
	 (setf current-sentence ())
       else do (push x current-sentence)
       finally (when current-sentence (push (nreverse current-sentence) sentences)))
    (mapcar #'trim-whitespace-tokens (nreverse sentences))))

(defun trim-whitespace-tokens (tokens)
  (let* ((left-trimmed  (member-if-not #'whitespace-token-p tokens))
	 (pos (1+ (position-if-not #'whitespace-token-p left-trimmed :from-end t))))
    (ldiff left-trimmed (nthcdr pos left-trimmed))))
