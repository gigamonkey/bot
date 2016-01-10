("purple" :template "What do you have that is purple?")
("green" :template "What do you eat that is green?")
("a *" :that "what do you * that is *" :template ("You " (thatstar 0) " a " (star) " that is " (thatstar 1) "?"))
("NICE TO MAKE YOUR ACQUAINTENCE" :template (process "NICE TO MEET YOU"))
("Q" :template (process "QUIT"))
("_ HELLO THERE" :template ("Greetings"))
("_ WILDCARD TEST" :template ("Successful wildcard test got: " (star)))
("A * C *" :template (dump))
("BOO" :template (random "Eeek!" "Yikes!" ("Boo, " "yourself.") (process "NICE TO MEET YOU")))
;("CASE MATCH *" :template ((think (set inp (star))) (case-match inp ("one" "uno") ("two" "dos") ("three" "tres") ("*" ((star) " is to mucho for me")))))
("CL WHAT *" :template (cond ((common-lisp-function-p (star)) "Yes, a function.") ((common-lisp-symbol-p (star)) "Yes, but not a function.") (t "Nope.")))
("CL *" :template (if (common-lisp-symbol-p (star)) "Yes." "No."))
;("COND *" :template (cond ((match (star) "one") "odin") ((match (star) "two") "dva") (t ((star) " is mnogo."))))
("DEAD SEXY BOOK" :template "Practical Common Lisp is the sexiest book I know.")
("DEAD-SEXY-BOOK" :template ((process "DEAD SEXY BOOK") " (By the way, I'm not minion; you don't need the hyphens.)"))
("DO YOU KNOW MY NAME" :template (if name "Yes." "Nope, sorry"))
("HELLO" :template ((think "I just said hello.") "Hello yourself, " (capitalize (get name))))
("HELLO MY NAME IS *" :template ("Oh, hello, " (capitalize (set name (star)))))
("HELLO MY NAME IS * AND I LIKE TO" :template ("Hello, " (capitalize (set name (star))) ". Why do you like to " (star 1) "?"))
("HELLO, WHAT IS YOUR NAME" :template ("Bob."))
("HELLO * HOW ARE * DOING" :template ("I'm doing fine, thanks."))
("I AM *" :template ((think (set name (star))) "Greetings, " (if (= name "peter seibel") "master" "stranger.")))
;("MAGIC WORD *" :template ((think (set magic-word (star))) (when (match magic-word "abracadabra") "You know the magic word. " "Welcome.")))
("NICE TO MEET YOU" :template ("Mutual, I'm sure."))
("QUIT" :template (quit "Okay, nice talking to you."))
;("SECRET *" :template ((think (set secret (star))) (unless (match secret "xyzzy") "You don't know the secret. " "Go away!")))
("THE QUICK BROWN FOX" :template ("Jumps over the lazy dog."))
("WHAT" :template ("I said: " (that)))
("WHAT IS THE FUNCTION TO * A *" :template (cond ((common-lisp-function-p (star 0)) ("Possibly " (uppercase (star 0)) ".")) ((common-lisp-function-p ((star 1) "-" (star 0))) ("It might be " (uppercase (star 1) "-" (star 0)) ".")) ((common-lisp-function-p ((star 0) "-" (star 1))) ("Maybe " (uppercase (star 0) "-" (star 1) "."))) (t "I don't know.")))
("WHO IS YOUR BOTMASTER" :template ("My " (get botmaster) " is " (get master) ".  " (think "      " (set he (get master)) "     ")))


;; Learning

("ADD PATTERN * WITH ANSWER *" :template ((add-rule (star 0) (star 1)) "Okay, thanks."))

("ADD A RULE" :template ((set-topic "ADD RULE PATTERN") "Okay. What's the pattern?"))
("_" :topic "ADD RULE PATTERN" :template ((think (set proposed-pattern (star))) (set-topic "ADD RULE ANSWER") "Okay. What should I say."))
("_" :topic "ADD RULE ANSWER" :template ((add-rule (get proposed-pattern) (star)) (clear-topic) "Got it. Thanks.")) 


;; Function

("TEST FUNCTION *" :template ("Here is the output of foo: " (function foo (star))))

("is not it grand" :template "It is grand.")
("isn't it nice" :template "It is nice.")
("I'd do it." :template "I bet you would.")
("I'd been there." :template "There?")
("I mean it." :template "No you don't.")
("I mean it!" :template "Okay, okay.")