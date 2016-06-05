(load #P"~/quicklisp/setup.lisp")
(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)
(in-package :cl-user)
(load "mcts.asd")
(load "cleaner-bot.asd")
(asdf::compile-system :cleaner-bot)
(asdf::load-system :cleaner-bot)

(print "cleaner-bot is loaded")



