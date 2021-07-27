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
  ("uint" (return (values '%uint 'uint)))
  ("int" (return (values '%int 'int)))
  ("memory" (return (values '%memory 'memory)))
  ("storage" (return (values '%storage 'storage)))
  ("calldata" (return (values '%calldata 'calldata)))
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
  (";" (return (values '|%;| '|;|)))
  )

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



(define-parser *sol-parser*
  (:start-symbol %source-unit)
  (:terminals (%pragma %number %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %uint %uint %memory %storage %calldata))

  (%source-unit
   (%source-unit-contents)
   (%source-unit-contents %source-unit)
   )

  (%source-unit-contents
   (%pragma-definition)
   (%contract-definition)
   )

  (%pragma-definition
   (%pragma #'(lambda (x) `(:pragma ,x))))

  (%contract-definition
   (%contract %identifier |%{| %contract-definition-contents |%}|
              #'(lambda (ctract id l-brak contents r-brak)
                  `(:contract ,id :contents ,contents)))
   )

  (%contract-definition-contents
   (%func-definition #'(lambda (x) `(:func-last ,x)))
   (%func-definition %contract-definition-contents
                     #'(lambda (a b) `(:func-head ,a :func-rest ,b)))
   )

  (%func-definition
   (%func %identifier |%(| %parameter-list |%)| %block ;; #'func_def_with_params
          #'(lambda (fun id l-brak par-lst r-brak blk)
              `(:fun ,id :params ,par-lst :blk ,blk)))
   (%func %identifier |%(| |%)| %block
          #'(lambda (fun id l-brak r-brak blk)
              `(:fun ,id :params '() :blk ,blk)))
   )

  (%parameter-list
   (%parameter #'(lambda (x) `(:par-last ,x)))
   (%parameter |%,| %parameter-list #'(lambda (a b c) `(:par-head ,a :par-rest ,c)))
   )

  (%parameter
   (%type-name #'(lambda (x) `(:par-type ,x)))
   )

  (%type-name
   (%uint #'(lambda (x) `(:type-name :uint)))
   ;; (%int)
   )

  (%block
   (|%{| |%}| #'(lambda (a b) `(:block-empty ,b)))
   (|%{| %statement |%}| #'(lambda (a b c) `(:block ,b)))
   )

  (%statement
   (%return-statement #'(lambda (x) `(:ret-stmt ,x)))
   )

  (%return-statement
   (%return %number |%;| #'(lambda (a b c) `(:ret ,b)))
   (%return |%;| #'(lambda (a b) `(:ret-empty)))
   )

  (%term
   %pragma %number %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %uint %int %memory %storage %calldata))

(progn
  (defparameter *clj* (sol-lexer (read-file-into-string "test1.sol")))
  (let ((result (parse-with-lexer *clj* *sol-parser*)))
    ;; (format t "~{~%~A~}" result)
    ;; (lddr result)
    (print result)
    ))
