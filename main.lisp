(in-package :cl-user)
(defpackage :cl-assembler/main
  (:use :common-lisp :cl-assembler/assembler)
  (:import-from :cl-assembler/symbol
                ;#:symboltable
                #:twopath
                #:solve_var
                #:cha_place)
  (:export #:parse #:solve_symbol))
(in-package :cl-assembler/main)

(defun parse (lis table ram)
  (let ((not_return_lis (subseq (clean lis) 0 (char_place (clean lis) #\Return))) ) 
    (case (commandtype not_return_lis)
      ((A_COMMAND)   (a_command (subseq not_return_lis 0 (char_place not_return_lis #\ )) table ram))
      ((C_COMMAND)   (c_command (subseq not_return_lis 0 (char_place not_return_lis #\ ))))
      ((L_COMMAND)   ) )))

(defun commandtype (lis)
  (let ((command (car lis)) )
    (cond ((string= command #\@) 'A_COMMAND)
          ((string= command #\() 'L_COMMAND)
          (t                     'C_COMMAND))))

(defun a_command (lis table ram)
  (let ((decimal (cdr lis))
        (type_b  (digit-char-p (cadr lis))) )
    (if type_b
        (format nil "~16,'0b" 
            (parse-integer (coerce decimal 'string)))
        (solve_var decimal table ram))
      ))

(defun solve_symbol (lis table rom)
  (twopath lis table rom))
