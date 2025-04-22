#!/usr/bin/cl --entry main

(setf *read-eval* nil)

(defun help ()
  (format t "Humanize sizes in bytes on stdin.~%")
  (format t "Uses base-two prefixes (kibibyte, Mebibyte... )~%")
  (format t "-k Pick which field to humanize, whitespace-separated, 1 based, default to first field.~%")
  (exit))

(defun prefmult (num base mult)
  (if (= num 0) 0
      (truncate (/ (log num base) mult))))

(defun truncate-to-result (num base mult)
  (truncate (/ num (expt base (* (prefmult num base mult) mult)))))

(defparameter *prefixes* '("B" "kiB" "MiB" "GiB" "TiB" "PiB" "EiB"))

(defun add-unit (num)
  (let ((pref (prefmult num 2 10)))
    (if (or (= pref 0) (>= pref (length *prefixes*)))
        (format nil "~a B" num)
        (let ((pre (nth pref *prefixes*)))
          (format nil "~a ~a" (truncate-to-result num 2 10) pre)))))

; Whitespace characters defined by Unicode for
; implementations which support it (e.g. CLISP, SBCL).
; (see http://www.unicode.org/Public/UCD/latest/ucd/PropList.txt)
; https://rosettacode.org/wiki/Strip_whitespace_from_a_string/Top_and_tail#Common_Lisp
(defvar *unicode-whitespace* 
  '(#\u0009 #\u000a #\u000b #\u000c #\u000d
    #\u0020 #\u0085 #\u00a0 #\u1680 #\u2000
    #\u2001 #\u2002 #\u2003 #\u2004 #\u2005
    #\u2006 #\u2007 #\u2008 #\u2009 #\u200a
    #\u2028 #\u2029 #\u202f #\u205f #\u3000))

(defun spacep (chr)
  (not (not (position chr *unicode-whitespace*))))

(defun split-spaces (str)
  (let ((res '())
        (chr '())
        (sstr ""))
    (dotimes (i (length str) (append res (list sstr)))
      (setq chr (char str i))
      (if (spacep chr)
          (if (> (length sstr) 0) (progn (setq res (append res (list sstr)))
                                         (setq sstr "")))
          (setq sstr (format nil "~a~a" sstr chr))))))

(defun split-fields (str)
  (let ((res '())
        (chr '())
        (sstr "")
        (pstr "")
        (n 1))
    (dotimes (i (length str) (values (append res (if (> (length pstr) 0) (list (list n pstr)) (if (> (length sstr) 0) (list (list 'X sstr)) '()))) n))
      (setq chr (char str i))
      (if (spacep chr)
          (progn (setq sstr (format nil "~a~a" sstr chr))
                 (if (> (length pstr) 0) (progn (setq res (append res (list (list n pstr))))
                                                (setq pstr "")
                                                (incf n))))
          (progn (setq pstr (format nil "~a~a" pstr chr))
                 (if (> (length sstr) 0) (progn (setq res (append res (list (list 'X sstr))))
                                                (setq sstr ""))))))))

(defun replace-field (fields n rep)
  (let ((res '()))
    (dolist (field fields res)
      (if (eq n (car field))
          (setq res (append res (list (list (car field) rep))))
          (setq res (append res (list field)))))))

(defun join-fields (fields)
  (let ((res ""))
    (dolist (field fields res)
      (setq res (format nil "~a~a" res (cadr field))))))

(defun map-fields (fields nums func)
  (let ((res '()))
    (dolist (field fields res)
      (if (find (car field) nums)
          (setq res (append res (list (list (car field) (funcall func (cadr field))))))
          (setq res (append res (list field)))))))

(defun find-fields (str)
  (let ((res '())
        (chr '())
        (tmp '()))
    (dotimes (i (length str) (if tmp (append res (list (append tmp (list (length str))))) res))
      (setq chr (char str i))
      (if (spacep chr)
          (if tmp (progn (setq res (append res (list (append tmp (list i)))))
                         (setq tmp '())))
          (if (not tmp) (setq tmp (list i)))))))

(defun flatten-fields (fields)
  (let ((res '()))
    (dolist (field fields res)
      (setq res (append res field)))))

(defun split-into-fields (str fields)
  (do* ((ffields (flatten-fields fields) (cdr ffields))
        (start (car ffields) (car ffields))
        (end (cadr ffields) (cadr ffields))
        (res (list (subseq str start end)) (append res (list (subseq str start end)))))
       ((< (length ffields) 2) res)))

(defun get-nth-field (str fields num)
  (if (> num (length fields))
      nil
      (subseq str (car (nth (- num 1) fields)) (cadr (nth (- num 1) fields)))))

(defun remove-nth-field (str fields num)
  (if (> num (length fields))
      nil
      (let* ((field (nth (- num 1) fields))
             (start (car field))
             (end (cadr field)))
        (list (subseq str 0 start) (subseq str end)))))

(defun replace-nth-field (sstr fields num rstr)
  (if (> num (length fields))
      nil
      (let* ((lst (remove-nth-field sstr fields num))
             (start (car lst))
             (end (cadr lst)))
        (format nil "~a~a~a" start rstr end))))

(defun get-field (lst num)
  (if (> num (length lst))
      nil
      (nth (- num 1) lst)))

(defun errout () (progn (format *error-output* "humanize: Invalid argument.~%") (exit :code 1)))

(defun parseint (str)
  (let ((res nil))
    (handler-case (setq res (parse-integer str :junk-allowed nil))
      (parse-error (condition) (progn condition nil)))
    res))

(defun doline (line fnums lnum)
  (block func
    (multiple-value-bind (fields nfields)
      (split-fields line)
    (dolist (n fnums)
      (if (> n nfields)
          (progn (format *error-output* "humanize: not enough fields on line ~a.~%" lnum)
                 (return-from func line)))
      (if (not (parseint (get-field fields n)))
          (progn (format *error-output* "humanize: field ~a (~a) not numeric on line ~a.~%" n (get-field fields n) lnum)
                 (return-from func line))))
      (join-fields (map-fields fields fnums (lambda (s) (add-unit (parseint s))))))))

(defun doit (field)
  (let ((lnum 1))
    (loop for str = (read-line *standard-input* nil)
          while str do (progn (format t "~a~%" (doline str field lnum)) (incf lnum))))
  (exit))

(defun main (argv)
  (let ((argc (length argv)))
    (cond ((= argc 0) (doit 1))
          ((= argc 1) (if (or (equal (car argv) "--help")
                              (equal (car argv) "-h")) (help)
                          (if (equal (subseq (car argv) 0 2) "-k")
                              (let ((field (parseint (subseq (car argv) 2))))
                                (if (or (not field) (<= field 0))
                                    (errout)
                                    (doit field))))))
          ((= argc 2)
           (if (equal (car argv) "-k")
               (let ((field (parseint (cadr argv))))
                 (if (or (not field) (<= field 0))
                     (errout)
                     (doit field)))
               (help)))
          (t (help)))))
