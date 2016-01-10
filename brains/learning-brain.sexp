;; Learning

("ADD PATTERN * WITH ANSWER *" :template ((add-rule (star 0) (star 1)) "Okay, thanks."))

("ADD A RULE" :template ((set-topic "ADD RULE PATTERN") "Okay. What's the pattern?"))
("_" :topic "ADD RULE PATTERN" :template 
     ((set-topic "ADD RULE ANSWER") (silent-set proposed-pattern (star))  "Okay. What should I say when someone says: '" (star) "'?"))
("_" :topic "ADD RULE ANSWER" :template ((add-rule (get proposed-pattern) (star)) (clear-topic) "Got it. Thanks.")) 


