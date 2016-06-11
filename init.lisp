(load #P"~/quicklisp/setup.lisp")
(proclaim '(optimize (debug 3) (safety 3)))
(require 'asdf)
(ql:quickload 'fset)
(ql:quickload 'lisp-unit)
(ql:quickload 'cl-quickcheck)
(ql:quickload 'alexandria)
(in-package :cl-user)
(asdf:initialize-source-registry '(:source-registry
                                   :inherit-configuration
                                   (:directory :here)
                                   (:directory (:here "src/"))))
(asdf::compile-system :cleaner-bot)
(asdf::load-system :cleaner-bot)

(print "cleaner-bot is loaded")



