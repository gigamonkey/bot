(in-package :com.gigamonkeys.bot)

(deftest pattern< ()
  (flet ((truly< (p1 p2)
	   (check (pattern< p1 p2)
		  (not (pattern< p2 p1)))))


  (check
    (not (pattern< '("_") '("_")))
    (not (pattern< '("X") '("X")))
    (not (pattern< '("*") '("*"))))
    
  (truly< '("_") '("X"))
  (truly< '("_") '("*"))
  (truly< '("A") '("B"))
  (truly< '("a") '("b"))
  (truly< '("A") '("b"))
  (truly< '("a") '("B"))

  (truly< '("_" "X") '("_"))
  (truly< '("*" "X") '("*"))
  (truly< '("a" "b") '("a")) ;; this doesn't really matter.
  ))




(deftest rules->trees ()
  (flet ((expect (in out)
	   (equal (rules->trees in) out)))
    (check
      ;; Empty list of rules to empty list of trees
      (expect '() '())
      
      ;; Empty rule is empty tree
      (expect '(()) '(()))

      ;; List of one trivial rule 
      (expect '((a)) '((a nil)))

      (expect '((a . 1)) '((a 1)))

      (expect '((a b c)) '((a (b (c nil)))))

      )))