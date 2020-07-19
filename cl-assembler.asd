(in-package :cl-user)
(defpackage :cl-assembler-asd
  (:use :cl :asdf))
(in-package :cl-assembler-asd)

(defsystem "cl-assembler"
  :class :package-inferred-system
  :version "0.1"
  :description "cl-assembler for nand2tetris"
  :license ""
  :depends-on ("cl-assembler/main")) 
