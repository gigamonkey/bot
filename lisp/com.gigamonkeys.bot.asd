;;
;; Copyright (c) 2010, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.bot
  :name "com.gigamonkeys.bot"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :components
  ((:file "packages")
   (:file "bot" :depends-on ("packages"))
   (:file "input-parsing" :depends-on ("packages"))
   (:file "circular-buffer" :depends-on ("packages"))
   (:file "irc" :depends-on ("packages"))
   (:file "web" :depends-on ("packages")))

  :depends-on 
  (:cl-irc
   :cl-ppcre
   :bordeaux-threads
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.test-framework
   :com.gigamonkeys.macro-utilities
   :com.gigamonkeys.utilities))
