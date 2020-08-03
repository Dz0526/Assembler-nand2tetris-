(in-package :cl-user)
(defpackage :cl-assembler/main
  (:use :common-lisp :cl-assembler/assembler)
  (:export #:parse))
(in-package :cl-assembler/main)

(defun parse (lis)
  (let ((not_return_lis (subseq lis 0 (char_place lis #\Return))) ) 
     (case (commandtype not_return_lis)
       ((A_COMMAND)   (a_command not_return_lis))
       ((C_COMMAND)   (c_command not_return_lis))
       ((L_COMMAND)   nil) )))



