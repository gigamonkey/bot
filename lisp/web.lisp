(in-package :com.gigamonkeys.bot)

(defparameter *web-bot* (make-instance 'bot :brain (make-instance 'file-brain :file #p"/home/peter/lisp/scratch/bot/gigabot-brain.sexp")))

(defun web-response (channel-id input responder)
  (let ((responses ()))
    (conversational-response (find-conversation *web-bot* channel-id) input (lambda (response) (push response responses)))
    (funcall responder (format nil "~{~a~^ ~}" (delete-duplicates (nreverse responses) :test #'string=)))))
