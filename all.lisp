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

  ("internal" (return (values '%visibility 'internal)))
  ("external" (return (values '%visibility 'external)))
  ("private" (return (values '%visibility 'private)))
  ("public" (return (values '%visibility 'public)))

  ("uint" (return (values '%type 'uint)))
  ("int" (return (values '%type 'int)))

  ("function" (return (values '%func 'func)))

  ("memory" (return (values '%data-location 'memory)))
  ("storage" (return (values '%data-location 'storage)))
  ("calldata" (return (values '%data-location 'calldata)))

  ("pure" (return (values '%state-mutability 'pure)))
  ("view" (return (values '%state-mutability 'view)))
  ("payable" (return (values '%state-mutability 'payable)))

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
  (:terminals (%pragma %number %visibility %state-mutability %returns %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %type %data-location))

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

   (%func %identifier %parlist %visibility %state-mutability %retlist %block
          #'(lambda (fun id parlist vis mutab retlist blk)
              `(:fun ,id :parlist ,parlist :visibility ,vis
                :state-mutability ,mutab
                :retlist ,retlist :blk ,blk)))

   (%func %identifier %parlist %visibility %retlist %block
          #'(lambda (fun id parlist vis retlist blk)
              `(:fun ,id :parlist ,parlist :visibility ,vis :retlist ,retlist :blk ,blk)))
   (%func %identifier %parlist %block
          #'(lambda (fun id parlist blk)
              `(:fun ,id :parlist ,parlist :blk ,blk)))
   (%func %identifier %parlist %retlist %block
          #'(lambda (fun id parlist retlist blk)
              `(:fun ,id :parlist ,parlist :retlist ,retlist :blk ,blk)))
   )

  (%parlist
   (|%(| |%)| #'(lambda (l-brak r-brak) `(:parlist nil)))
   (|%(| %parameter-list |%)| #'(lambda (l-brak parlist r-brak) `(:parlist ,parlist)))
   )

  (%retlist
   (%returns |%(| |%)| #'(lambda (ret l-brak r-brak) `(:retlist nil)))
   (%returns |%(| %parameter-list |%)| #'(lambda (ret l-brak retlist r-brak)
                                           `(:retlist ,retlist)))
   )

  (%parameter-list
   (%parameter #'(lambda (x) `(:par-last ,x)))
   (%parameter |%,| %parameter-list #'(lambda (a b c) `(:par-head ,a :par-rest ,c)))
   )

  (%parameter
   (%type #'(lambda (x) `(:par-type ,x)))
   (%type %data-location #'(lambda (a b) `(:par-type ,a :data-location ,b)))
   (%type %data-location %identifier
          #'(lambda (a b c) `(:par-type ,a :data-location ,b :name ,c)))
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
   %pragma %number %visibility %state-mutability %returns %return |%;| |%{| |%}| %contract %func |%(| |%)| %identifier |%,| %type %data-location))

(progn
  (defparameter *clj* (sol-lexer (read-file-into-string "test1.sol")))
  (let ((result (parse-with-lexer *clj* *sol-parser*)))
    ;; (format t "~{~%~A~}" result)
    ;; (lddr result)
    (print result)
    ))
