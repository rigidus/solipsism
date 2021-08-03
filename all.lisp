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
  ("returns" (return (values '%returns 'returns)))
  ("return" (return (values '%return 'return)))
  ("-?0|[1-9][0-9]*(\\.[0-9]*)?([e|E][+-]?[0-9]+)?"
   (return (values '%number (read-from-string $@))))
  ("[a-zA-Z0-9_]+" (return (values '%identifier $@)))
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
  (:terminals (%pragma %number %returns %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %uint %int %memory %storage %calldata))

  (%source-unit
   (%source-unit-contents #'(lambda (x) `(:src-last ,x)))
   (%source-unit-contents %source-unit #'(lambda (a b) `(:src-head ,a :src-rest, b)))
   )

  (%source-unit-contents
   (%pragma-definition #'(lambda (x) `(:pragma-def ,x)))
   (%contract-definition #'(lambda (x) `(:contract-def ,x)))
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
   (%func %identifier |%(| |%)| %retlist %block
          #'(lambda (fun id l-brak r-brak retlist blk)
              `(:fun ,id :params '() :retlist ,retlist :blk ,blk)))
   )

  (%retlist
   (%returns |%(| |%)| #'(lambda (ret l-brak r-brak) `(:retlist nil)))
   (%returns |%(| %parameter-list |%)| #'(lambda (ret l-brak ret-lst r-brak)
                                           `(:retlist ,ret-lst)))
   )

  (%parameter-list
   (%parameter #'(lambda (x) `(:par-last ,x)))
   (%parameter |%,| %parameter-list #'(lambda (a b c) `(:par-head ,a :par-rest ,c)))
   )

  (%parameter
   (%type-name #'(lambda (x) `(:par-type ,x)))
   (%type-name %data-location #'(lambda (a b) `(,@a ,@b)))
   (%type-name %data-location %identifier #'(lambda (a b c) `(,@a ,@b :name ,c)))
   )

  (%type-name
   (%uint #'(lambda (x) `(:type-name :uint)))
   (%int #'(lambda (x) `(:type-name :int)))
   ;; ...
   )

  (%data-location
   (%memory #'(lambda (x) `(:data-location :memory)))
   (%storage #'(lambda (x) `(:data-location :storage)))
   (%calldata #'(lambda (x) `(:data-location :calldata)))
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
   %pragma %number %returns %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %uint %int %memory %storage %calldata))

(progn
  (defparameter *clj* (sol-lexer (read-file-into-string "test1.sol")))
  (let ((result (parse-with-lexer *clj* *sol-parser*)))
    ;; (format t "~{~%~A~}" result)
    ;; (lddr result)
    (print result)
    ))
