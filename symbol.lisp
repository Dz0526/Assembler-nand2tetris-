(in-package :cl-user)
(defpackage :cl-assembler/symbol
  (:use :common-lisp)
  (:export ))
(in-package :cl-assembler/symbol)

(symboltable)

(gethash 'SP symboltable)

(defun symboltable ()
  (defparameter symboltable (make-hash-table))
  (setf (gethash 'SP   symboltable) 0)
  (setf (gethash 'LCL  symboltable) 1)
  (setf (gethash 'ARG  symboltable) 2)
  (setf (gethash 'THIS symboltable) 3)
  (setf (gethash 'THAT symboltable) 4)
  (setf (gethash 'R0   symboltable) 0)
  (setf (gethash 'R1   symboltable) 1)
  (setf (gethash 'R2   symboltable) 2)
  (setf (gethash 'R3   symboltable) 3)
  (setf (gethash 'R4   symboltable) 4)
  (setf (gethash 'R5   symboltable) 5)
  (setf (gethash 'R6   symboltable) 6)
  (setf (gethash 'R7   symboltable) 7)
  (setf (gethash 'R8   symboltable) 8)
  (setf (gethash 'R9   symboltable) 9)
  (setf (gethash 'R10  symboltable) 10)
  (setf (gethash 'R11  symboltable) 11)
  (setf (gethash 'R12  symboltable) 12)
  (setf (gethash 'R13  symboltable) 13)
  (setf (gethash 'R14  symboltable) 14)
  (setf (gethash 'R15  symboltable) 15)
  (setf (gethash 'SCREEN  symboltable) 16384)
  (setf (gethash 'KBD  symboltable) 24576))

(defun twopath (lis)
  (let ((fir (caar lis)) )
   (cond ((string= fir #\()      (l_command (car lis))
                                 (twopath (cdr lis)))
         ((string= fir #\/)      (twopath (cdr lis)))
         ((string= fir #\Return) (twopath (cdr lis)))
         (t                      (funcall counter_rom) (twopath (cdr lis))))))

(defun l_command (lis)
  (let ((sym (subseq lis 1 (char_place lis #\))) ))
   (addEntry symboltable (intern (concatenate 'string sym)) (funcall counter_rom))))

(defun addEntry (table sym address)
  (setf (gethash sym table) address))

(defun contains (table sym)
  (let ((bool (gethash sym table)) )
    (if bool
        t
        nil)))

(defun getAddress (table sym)
  (gethash sym table))

(defun make_counter (start)
  (let ((cnt start)) 
    (lambda () (incf cnt))))
