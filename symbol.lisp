(in-package :cl-user)
(defpackage :cl-assembler/symbol
  (:use :common-lisp)
  (:export ;#:symboltable
           #:twopath
           #:solve_var
           #:cha_place))
(in-package :cl-assembler/symbol)

#|
(defun symboltable ()
  (defparameter *symboltable* (make-hash-table))
  (setf (gethash (intern "SP")   *symboltable*) 0)
  (setf (gethash (intern "LCL")  *symboltable*) 1)
  (setf (gethash (intern "ARG")  *symboltable*) 2)
  (setf (gethash (intern "THIS") *symboltable*) 3)
  (setf (gethash (intern "THAT") *symboltable*) 4)
  (setf (gethash (intern "R0")   *symboltable*) 0)
  (setf (gethash (intern "R1")   *symboltable*) 1)
  (setf (gethash (intern "R2")   *symboltable*) 2)
  (setf (gethash (intern "R3")   *symboltable*) 3)
  (setf (gethash (intern "R4")   *symboltable*) 4)
  (setf (gethash (intern "R5")   *symboltable*) 5)
  (setf (gethash (intern "R6")   *symboltable*) 6)
  (setf (gethash (intern "R7")   *symboltable*) 7)
  (setf (gethash (intern "R8")   *symboltable*) 8)
  (setf (gethash (intern "R9")   *symboltable*) 9)
  (setf (gethash (intern "R10")  *symboltable*) 10)
  (setf (gethash (intern "R11")  *symboltable*) 11)
  (setf (gethash (intern "R12")  *symboltable*) 12)
  (setf (gethash (intern "R13")  *symboltable*) 13)
  (setf (gethash (intern "R14")  *symboltable*) 14)
  (setf (gethash (intern "R15")  *symboltable*) 15)
  (setf (gethash (intern "SCREEN")  *symboltable*) 16384)
  (setf (gethash (intern "KBD")  *symboltable*) 24576))
|#

(defun twopath (lis table rom)
  (let ((pwd (caar lis)) 
        (fir (caadr lis)))
   (cond ((string= fir #\()      (l_command (cadr lis) table rom)
                                 (twopath (cdr lis) table rom))
         ((string= pwd #\()      (twopath (cdr lis) table rom))
         ((string= fir #\/)      (twopath (cdr lis) table rom))
         ((string= fir #\Return) (twopath (cdr lis) table rom))
         ((eq      fir nil)      nil)
         (t                      (funcall rom) (twopath (cdr lis) table rom)))))

(defun l_command (lis table rom)
  (let ((sym (subseq lis 1 (cha_place lis #\))) ))
   (addEntry table (intern (concatenate 'string sym)) (funcall rom))))


(defun solve_var (lis table ram)
  (let ((sym (intern (concatenate 'string lis)) ))
    (if (contains table sym)
        (format nil "~16,'0b" (getAddress table sym))  
        (progn (addEntry table sym (funcall ram))
               (format nil "~16,'0b" (getAddress table sym)) ) 
        )) )


(defun addEntry (table sym address)
  (setf (gethash sym table) address))

(defun contains (table sym)
  (let ((bool (gethash sym table)) )
    (if bool
        t
        nil)))


(defun getAddress (table sym)
  (gethash sym table))


(defun cha_place (lis charr)
 (let ((searchc (car lis) )
       (bool    (find charr lis)))
   (when bool
     (if (string= searchc charr)
     0
     (1+ (cha_place (cdr lis) charr))))))
