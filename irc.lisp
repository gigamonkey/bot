(in-package :com.gigamonkeys.bot)

(defparameter *default-nick* "gigabot")
(defparameter *default-server* "irc.freenode.net")
(defparameter *default-user* "gigamonkey")
(defparameter *default-brain-file* "gigabot-brain.sexp")

(defparameter *nick-regexp* (create-scanner "^([A-Za-z\\[\\]\\\\`_^{|}][A-Za-z0-9\\[\\]\\\\`_^{|}]{0,8}): (.*)$"))

(defvar *process-count* 0)
(defvar *bots* ())

(defclass irc-bot (bot)
  ((nickname :initarg :nickname :accessor nickname)
   (channel :initarg :channel :accessor channel)
   (connection :initarg :connection :accessor connection)
   (thread :initarg :thread :accessor thread)
   (history :initform (make-instance 'circular-buffer :size 100) :accessor history)))

(defmethod make-conversation ((bot irc-bot))
  (let ((c (call-next-method)))
    (add-function c 'last-thing-said-by
		  (lambda (who) (last-thing-said-by bot who)))
    (add-function c 'last-thing-said-by-before
		  (lambda (who what) (last-thing-said-by-before bot who what)))
    (add-function c 'last-thing-said-by-about
		  (lambda (who what) (last-thing-said-by-before bot who what)))
    
    c))

(defun start-bot (channel &key 
		  (nick *default-nick*)
		  (server *default-server*)
		  (user *default-user*)
		  (brain-file *default-brain-file*))
  (let* ((connection (connect :nickname nick :server server :username user))
	 (bot
	  (make-instance 'irc-bot
	    :nickname nick
	    :channel channel
	    :brain (make-instance 'file-brain :file brain-file)
	    :connection connection)))
    (join connection channel)
    (add-hook connection 'irc::irc-privmsg-message (make-message-hook bot))
    (run-message-loop bot)
    (push bot *bots*)
    bot))

(defun reset-message-hook (bot)
  (with-slots (connection) bot
    (remove-hooks connection 'irc::irc-privmsg-message)
    (add-hook connection 'irc::irc-privmsg-message (make-message-hook bot))))
  

(defun stop-bot (&optional (bot (first *bots*)))
  (quit (connection bot))
  (destroy-thread (thread bot)))

(defun load-brain-file (file)
  (make-instance 'file-brain :file file))

(defun reload-brain-file (&optional (bot (first *bots*)))
  (reload-brain (brain bot)))

(defun run-message-loop (bot)
  (with-slots (connection thread) bot
    (setf thread (start-background-message-loop connection))))

(defun change-nick (bot new-nick)
  (nick (connection bot) (setf (nickname bot) new-nick)))

(defun addressee (msg)
  "Split the text of `msg' into a possibly nil addressee (a nick) and
the rest of the message."
  (multiple-value-bind (ms me rs re) (scan *nick-regexp* msg)
    (if (and ms me)
	(let ((nick (subseq msg (aref rs 0) (aref re 0)))
	      (msg (subseq msg (aref rs 1) (aref re 1))))
	  (values nick msg))
	(values nil msg))))
  
(defun make-message-hook (bot)
  (lambda (message)
    (format t "~&Got message: ~a" message)
    (multiple-value-bind (speaker destination addressee text) (parse-message message)
      (format t "~&speaker: ~a; destination: ~a; addressee: ~a; text: ~a" speaker destination addressee text)
      (add-item (list speaker destination addressee text) (history bot))
      (let ((conversation (find-conversation bot speaker))
	    (nick (nickname bot)))
	(cond
	  ((equal addressee nick)
	   (format t "~&Channel mesage addressed to bot. ~a" addressee)
	   ;; Messages on channel addressed to bot or private messages
	   ;; to bot
	   (irc-respond bot text destination conversation))
	
	  ((equal destination nick)
	   (format t "~&Private message to bot. (equal ~s ~s)" destination nick)
	   ;; Private message to bot. Reply directly to speaker.
	   (irc-respond bot text speaker conversation))
	
	  ((and addressee (not (equal addressee nick)))
	   (format t "~&Channel message not for us.")
	   ;; On channel, addressed to someone other than bot. Definitely
	   ;; not for us. (Though later we may want to eavesdrop on these
	   ;; and try to pick things up.)
	   nil)
	
	  ((and (not (equal destination nick)) (not addressee))
	   (format t "~&Channel message possibly for us.")
	   ;; On channel, not addressed to anyone in particular. Could be
	   ;; to us. Find out from the conversation
	   (when (in-exchange-p conversation)
	     (format t "~&Channel message while in-exchange-p with ~a" speaker)
	     ;; The conversation knows we're in a potential exchange (a
	     ;; multi-step conversation)
	     (irc-respond bot text destination conversation))))))
    t))

(defun parse-message (message)
  (let ((source (source message))
	(arguments (arguments message)))
    (destructuring-bind (destination msg) arguments
      (multiple-value-bind (addressee text) (addressee msg)
	(values source destination addressee text)))))

(defun last-thing-said-by (bot speaker)
  (map-items-backward (lambda (item)
			(when (string-equal speaker (first item))
			  (return-from last-thing-said-by (fourth item))))
		      (history bot)))

(defun last-thing-said-by-before (bot speaker what)
  (let ((next-one nil))
    (map-items-backward 
     (lambda (item)
       (when (string-equal speaker (first item))
	 (cond
	   ((string-equal (fourth item) what)
	    (setf next-one t))
	   (next-one
	    (return-from last-thing-said-by-before (fourth item))))))
     (history bot))))

(defun last-thing-said-by-about (bot speaker what)
  (let ((scanner (create-scanner what :case-insensitive-mode t)))
    (block nil
      (map-items-backward 
       (lambda (item)
       (when (and (string-equal speaker (first item))
		  (scan scanner (fourth item)))
	 (return  (fourth item))))
       (history bot)))))

(defun irc-respond (bot input destination conversation)
  (let ((responses ()))
    (conversational-response  conversation input (lambda (response) (push response responses)))
    (let ((response (format nil "~{~a~^ ~}" (delete-duplicates (nreverse responses) :test #'string=))))
      (privmsg (connection bot) destination response))))

(defun start-background-message-loop (connection)
  "Read messages from the `connection', parse them and dispatch
irc-message-event on them. Returns background process ID if available.

This function has been copied, more or less, from cl-irc's
protocol.lisp. As they say there, we should add some better error
handling."
  (flet ((message-loop ()
	   (format t "~&Started message loop.")
	   (finish-output)
	   (loop
	      (handler-bind ((no-such-reply #'continue))
		(read-message-loop connection)))))
    (let ((name (format nil "irc-handler-~D" (incf *process-count*))))
      (make-thread #'message-loop :name name))))

(defun dump-message-info (message)
  (format t "~&Got message: ~s" message)
  (format t "~&~2tsource: ~s" (source message)) ;; the nick of the sender
  (format t "~&~2tuser: ~s" (user message))
  (format t "~&~2thost: ~s" (host message))
  (format t "~&~2tcommand: ~s" (command message)) ;; should always by PRIVMSG
  (format t "~&~2targuments: ~s" (arguments message)) ;; (channel/user text)
  (format t "~&~2tconnection: ~s" (connection message))
  (format t "~&~2treceived-time: ~s" (received-time message))
  (format t "~&~2traw-message-string: ~s" (raw-message-string message))
  (format t "~&~2tself-message-p: ~s" (self-message-p message)))
  
