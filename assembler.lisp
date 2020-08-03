(in-package :cl-user)
(defpackage :cl-assembler/assembler
  (:use :common-lisp)
  (:export #:commandtype
           #:a_command
           #:c_command
           #:char_place))
(in-package :cl-assembler/assembler)


(defun commandtype (lis)
  (let ((command (car lis)) )
    (cond ((string= command #\@) 'A_COMMAND)
          ((string= command #\() 'L_COMMAND)
          (t                     'C_COMMAND))))

(defun a_command (lis)
  (let ((decimal (cdr lis)) )
    (format nil "~16,'0b" 
            (parse-integer (coerce  decimal 'string)))  ))

(defun c_command (lis)
  (let ((dest_p (dest_find lis))
        (comp_p (comp_find lis))
        (jmp_p  (jmp_find  lis)))
   (format nil "111~d~d~d" (c_comp comp_p) (c_dest dest_p) (c_jump jmp_p)) ))


(defun char_place (lis charr)
 (let ((searchc (car lis) )
       (bool    (find charr lis)))
   (when bool
     (if (string= searchc charr)
     0
     (1+ (char_place (cdr lis) charr))))))


(defun dest_find (lis)
  (let ((find=  (char_place lis #\=)) )
   (if find=
       (subseq lis 0 find=)
       nil) ))


(defun comp_find (lis)
  (let ((find=  (char_place lis #\=))
        (find_c (char_place lis #\;)))
   (cond ((and find= find_c) (subseq lis (1+ find=) find_c))
         (find=              (subseq lis (1+ find=)))
         (find_c             (subseq lis 0 find_c))
         (t                  lis)
         ) ))


(defun jmp_find (lis)
  (let ((find_c (char_place lis #\;)) )
    (if find_c
        (subseq lis (1+ find_c))
        nil)))


(defun c_dest (lis)
  (let ((dest (coerce lis 'string)) )
    (cond 
      ((string= dest "M")   "001")
      ((string= dest "D")   "010")
      ((string= dest "MD")  "011")
      ((string= dest "A")   "100")
      ((string= dest "AM")  "101") 
      ((string= dest "AD")  "110")
      ((string= dest "AMD") "111")
      (t                    "000"))))

(defun c_comp (lis)
  (let ((comp (coerce lis 'string)) )
    (cond
      ((string= comp "0")     "0101010") 
      ((string= comp "1")     "0111111")
      ((string= comp "-1")    "0111010")
      ((string= comp "D")     "0001100")
      ((string= comp "A")     "0110000")
      ((string= comp "!D")    "0001101")
      ((string= comp "!A")    "0110001")
      ((string= comp "-D")    "0001111")
      ((string= comp "-A")    "0110011")
      ((string= comp "D+1")   "0011111")
      ((string= comp "A+1")   "0110111")
      ((string= comp "D-1")   "0001110")
      ((string= comp "A-1")   "0110010")
      ((string= comp "D+A")   "0000010")
      ((string= comp "D-A")   "0010011")
      ((string= comp "A-D")   "0000111")
      ((string= comp "D&A")   "0000000")
      ((string= comp "D|A")   "0010101")

      ((string= comp "M")     "1110000")
      ((string= comp "!M")    "1110001")
      ((string= comp "-M")    "1110011")
      ((string= comp "M+1")   "1110111")
      ((string= comp "M-1")   "1110010")
      ((string= comp "D+M")   "1000010")
      ((string= comp "D-M")   "1010011")
      ((string= comp "M-D")   "1000111")
      ((string= comp "D&M")   "1000000")
      ((string= comp "D|M")   "1010101"))))

(defun c_jump (lis)
  (let ((jmp (coerce lis 'string)) )
    (cond 
      ((string= jmp "JGT")  "001")
      ((string= jmp "JEQ")  "010")
      ((string= jmp "JGE")  "011")
      ((string= jmp "JLT")  "100")
      ((string= jmp "JNE")  "101")
      ((string= jmp "JLE")  "110")
      ((string= jmp "JMP")  "111")
      (t                    "000"))))

