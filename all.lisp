(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:yacc :cl-lex :alexandria :anaphora)))

(defpackage :sol-parser
  (:use :cl :yacc :cl-lex :alexandria :anaphora))

(in-package :sol-parser)

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defun lddr (par)
  (if (equal 2 (length par))
      (cons (car par) (lddr (cadr par)))
      par))

(define-string-lexer sol-lexer
  ;; ("//(.*)" (return (values '%comment $@)))
  ;; ("(?s)/\\*(.*)\\*/" (values 'multiline-comment $@)) ;; TODO
  ("\"([^\\\"]|\\.)*?\"" (return (values '%string (string-trim "\"" $@))))
  ("true" (return (values '%true 'true)))
  ("false" (return (values '%false 'false)))
  ("contract" (return (values '%contract 'contract)))
  ("function" (return (values '%func 'func)))
  ("pragma\\s+([^;]|\\.)*;" (return (values '%pragma (subseq $@ 7))))
  ("\\(" (return (values '|%(| '|(|)))
  ("\\)" (return (values '|%)| '|)|)))
  ("{" (return (values '|%{| '{)))
  ("}" (return (values '|%}| '})))
  ("," (return (values '|%,| '|,|)))
  ("return" (return (values '%return 'return)))
  ("-?0|[1-9][0-9]*(\\.[0-9]*)?([e|E][+-]?[0-9]+)?"
   (return (values '%number (read-from-string $@))))
  ("[a-zA-Z0-9_]+" (return (values '%identifier (read-from-string $@))))
  (";" (return (values '|%;| '|;|))))

(defparameter *clj* (sol-lexer (read-file-into-string "test1.sol")))

;; main
(tagbody
 repeat
   (multiple-value-bind (one two)
       (funcall *clj*)
     (format t "~%~A : ~A" one (bprint two))
     (when one
       (go repeat)))
   'fin)

(defun func_definition_rule (fun id l-brak par-lst r-brak blk)
  (declare (ignore fun l-brak r-brak))
  (let ((res))
    (block ppp
      (labels ((tst (lst)
                 (if (null lst)
                     (return-from ppp)
                     (progn
                       (push (car lst) res)
                       (tst (caddr lst))))))
        (tst par-lst)))
    `(:fun ,id ,(reverse res) ,blk)))

(define-parser *sol-parser*
  (:start-symbol %source-unit)
  (:terminals (%pragma %number %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,|))

  (%source-unit
   (%source-unit-contents)
   (%source-unit-contents %source-unit)
   )

  (%source-unit-contents
   %pragma-definition
   %contract-definition
   )

  (%pragma-definition
   (%pragma #'(lambda (x) `(:pragma ,x))))

  (%contract-definition
   (%contract %identifier |%{| %contract-definition-contents |%}|)
   )

  (%contract-definition-contents
   (%func-definition)
   (%func-definition %contract-definition-contents)
   )

  (%func-definition
   (%func %identifier |%(| %parameter-list |%)| %block #| #'func_definition_rule |# )
   (%func %identifier |%(| |%)| %block #| #'func_definition_rule |# )
   )

  (%parameter-list
   (%identifier)
   (%identifier |%,| %parameter-list))

  (%block
   (|%{| |%}|)
   (|%{| %statement |%}|)
   )

  (%statement
   %return-statement
   )

  (%return-statement
   (%return %number |%;|)
   (%return |%;|)
   )

  (%term
   %pragma %number %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| ))

(progn
  (defparameter *clj* (sol-lexer (read-file-into-string "test1.sol")))
  (let ((result (parse-with-lexer *clj* *sol-parser*)))
    ;; (format t "~{~%~A~}" result)
    result))
