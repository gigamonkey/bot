(in-package :cl-user)

(defpackage :com.gigamonkeys.bot
  (:use :cl :cl-ppcre
	:com.gigamonkeys.test
	:com.gigamonkeys.utilities
	:com.gigamonkeys.pathnames
	:com.gigamonkeys.macro-utilities
	:irc
	:bordeaux-threads))

(defpackage :com.gigamonkeys.bot.templates
  (:use :cl :com.gigamonkeys.bot)
  (:import-from :com.gigamonkeys.bot :that :topic)
  (:shadow :random))




