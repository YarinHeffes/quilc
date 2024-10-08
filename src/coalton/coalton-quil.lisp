;;;; Author: Yarin Heffes

(defpackage #:coalton-quil
  (:documentation
   "This package defines an interface between Coalton and the Common Lisp library `cl-quil`.")
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:tree #:coalton-library/ord-tree)
   (#:map #:coalton-library/ord-map))
  (:export
   #:QuilMemoryRef
   #:QuilMemoryDescriptor
   #:QuilGateApplication
   #:QuilMeasure
   #:QuilMeasureDiscard
   #:QuilMeasurement
   #:QuilClassicalMove
   #:QuilClassicalExclusiveOr
   #:QuilBinaryClassicalInstruction
   #:QuilPragma
   #:QuilHalt
   #:QuilInstruction
   #:QuilNamedOperator
   #:QuilDaggerOperator
   #:QuilOperatorDescription
   #:QuilParsedProgram
   #:QuilExecutableCode
   #:QuilChipSpecification
   #:QuilRewiring
   #:parse-quil
   #:parse-file
   #:parse-qasm
   #:get-parsed-program-memory-definitions
   #:set-parsed-program-memory-definitions!
   #:map-parsed-program-memory-definitions!
   #:get-parsed-program-executable-code
   #:set-parsed-program-executable-code!
   #:map-parsed-program-executable-code!
   #:parsed-program-multi-qubit-depth
   #:get-parsed-program-highest-qubit-index
   #:copy-parsed-program
   #:print-parsed-program
   #:get-parsed-program-final-rewiring
   #:get-quil-gate-application-qubits
   #:get-quil-gate-application-angle
   #:get-quil-measure-qubit
   #:get-quil-measure-discard-qubit
   #:get-quil-measurement-qubit
   #:get-quil-measure-address
   #:get-quil-measurement-address
   #:get-quil-instruction-qubits
   #:get-quil-operator-description
   #:get-quil-named-operator-name
   #:get-quil-dagger-operator-operator
   #:make-quil-gate-application
   #:make-quil-measurement
   #:make-quil-classical-move
   #:make-quil-classical-exclusive-or
   #:make-quil-memory-descriptor
   #:make-quil-memory-ref
   #:compiler-hook
   #:nativize-executable-code
   #:parsed-program-metrics
   #:get-chip-specification-links
   #:build-IBM-Qx5
   #:build-nQ-fully-connected-chip
   #:build-nQ-foust-chip
   #:get-quil-rewiring-l2p
   #:get-quil-rewiring-p2l))

(in-package #:coalton-quil)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (repr :native cl-quil:memory-ref)
  (define-type QuilMemoryRef)

  (repr :native cl-quil:memory-descriptor)
  (define-type QuilMemoryDescriptor)

  (repr :native cl-quil:gate-application)
  (define-type QuilGateApplication)

  (repr :native cl-quil:measure)
  (define-type QuilMeasure)

  (repr :native cl-quil:measure-discard)
  (define-type QuilMeasureDiscard)

  (repr :native cl-quil:measurement)
  (define-type ClQuilMeasurement)

  (repr :native cl-quil:classical-move)
  (define-type QuilClassicalMove)

  (repr :native cl-quil:classical-exclusive-or)
  (define-type QuilClassicalExclusiveOr)

  (repr :native cl-quil:binary-classical-instruction)
  (define-type ClQuilBinaryClassicalInstruction)

  (repr :native cl-quil:pragma)
  (define-type QuilPragma)

  (repr :native cl-quil:halt)
  (define-type QuilHalt)

  (repr :native cl-quil:instruction)
  (define-type ClQuilInstruction)

  (repr :native cl-quil:named-operator)
  (define-type QuilNamedOperator)

  (repr :native cl-quil:dagger-operator)
  (define-type QuilDaggerOperator)

  (repr :native cl-quil:operator-description)
  (define-type ClQuilOperatorDescription)

  (repr :native cl-quil:parsed-program)
  (define-type QuilParsedProgram)

  (repr :native cl:simple-vector)
  (define-type ClQuilExecutableCode)

  (repr :native cl-quil::chip-specification)
  (define-type QuilChipSpecification)

  (repr :native cl:hash-table)
  (define-type QuilGateInformation)

  (repr :native cl-quil:rewiring)
  (define-type QuilRewiring))

(coalton-toplevel

  (define-type QuilMeasurement
    (QuilMeasure QuilMeasure)
    (QuilMeasureDiscard QuilMeasureDiscard))

  (define-instance (Into ClQuilMeasurement QuilMeasurement)
    (define (into measurement-m)
      (lisp QuilMeasurement (measurement-m)
        (adt:match cl-quil:measurement measurement-m
          ((cl-quil:measure)
           (coalton (QuilMeasure (lisp QuilMeasure () measurement-m))))
          ((cl-quil:measure-discard)
           (coalton (QuilMeasureDiscard (lisp QuilMeasureDiscard () measurement-m))))
          (_ (coalton (error "Unexcepted cl-quil:measurement.

Must be in {cl-quil:measure, cl-quil:measure-discard}.")))))))
  (define-instance (Into QuilMeasurement ClQuilMeasurement)
    (define (into measurement-m)
      (match measurement-m
        ((QuilMeasure measure-m)
         (lisp ClQuilMeasurement (measure-m) measure-m))
        ((QuilMeasureDiscard measure-discard-m)
         (lisp ClQuilMeasurement (measure-discard-m) measure-discard-m)))))
  (define-instance (Iso QuilMeasurement ClQuilMeasurement)))

(coalton-toplevel

  (define-type QuilBinaryClassicalInstruction
    (QuilClassicalMove QuilClassicalMove)
    (QuilClassicalExclusiveOr QuilClassicalExclusiveOr))

  (define-instance (Into ClQuilBinaryClassicalInstruction QuilBinaryClassicalInstruction)
    (define (into instruction-i)
      (lisp QuilBinaryClassicalInstruction (instruction-i)
        (adt:match cl-quil:binary-classical-instruction instruction-i
          ((cl-quil:classical-move)
           (coalton (QuilClassicalMove (lisp QuilClassicalMove () instruction-i))))
          ((cl-quil:classical-exclusive-or)
           (coalton (QuilClassicalExclusiveOr (lisp QuilClassicalExclusiveOr () instruction-i))))
          (_ (coalton (error "Unexpected cl-quil:classical-instruction.

Must be in {cl-quil:classical-move, cl-quil:classical-exclusive-or}.")))))))
  (define-instance (Into QuilBinaryClassicalInstruction ClQuilBinaryClassicalInstruction)
    (define (into instruction-i)
      (match instruction-i
        ((QuilClassicalMove classical-move-m)
         (lisp ClQuilBinaryClassicalInstruction (classical-move-m) classical-move-m))
        ((QuilClassicalExclusiveOr classical-exclusive-or-x)
         (lisp ClQuilBinaryClassicalInstruction (classical-exclusive-or-x) classical-exclusive-or-x)))))
  (define-instance (Iso QuilBinaryClassicalInstruction ClQuilBinaryClassicalInstruction)))

(coalton-toplevel

  (define-instance (Into ClQuilMeasurement ClQuilInstruction)
    (define (into measurement-m)
      (lisp ClQuilInstruction (measurement-m) measurement-m)))

  (define-instance (Into ClQuilBinaryClassicalInstruction ClQuilInstruction)
    (define (into classical-instruction-i)
      (lisp ClQuilInstruction (classical-instruction-i) classical-instruction-i)))

  (define-type QuilInstruction
    (QuilGateApplication QuilGateApplication)
    (QuilMeasurement QuilMeasurement)
    (QuilBinaryClassicalInstruction QuilBinaryClassicalInstruction)
    (QuilPragma QuilPragma)
    (QuilHalt QuilHalt))

  (define-instance (Into ClQuilInstruction QuilInstruction)
    (define (into instruction-i)
      (lisp QuilInstruction (instruction-i)
        (adt:match cl-quil:instruction instruction-i
          ((cl-quil:gate-application)
           (coalton (QuilGateApplication (lisp QuilGateApplication () instruction-i))))
          ((cl-quil:measurement)
           (coalton (QuilMeasurement (into (lisp ClQuilMeasurement () instruction-i)))))
          ((cl-quil:binary-classical-instruction)
           (coalton (QuilBinaryClassicalInstruction (into (lisp ClQuilBinaryClassicalInstruction () instruction-i)))))
          ((cl-quil:pragma)
           (coalton (QuilPragma (lisp QuilPragma () instruction-i))))
          ((cl-quil:halt)
           (coalton (QuilHalt (lisp QuilHalt () instruction-i))))
          (_ (coalton (error "Unexpected cl-quil:instruction.

Must be in {cl-quil:gate-application, cl-quil:measurement, cl-quil:binary-classical-instruction, cl-quil:pragma, cl-quil:halt}.")))))))
  (define-instance (Into QuilInstruction ClQuilInstruction)
    (define (into instruction-i)
      (match instruction-i
        ((QuilGateApplication gate-application-g)
         (lisp ClQuilInstruction (gate-application-g) gate-application-g))
        ((QuilMeasurement measurement-m)
         (into (as ClQuilMeasurement measurement-m)))
        ((QuilBinaryClassicalInstruction classical-instruction-i)
         (into (as ClQuilBinaryClassicalInstruction classical-instruction-i)))
        ((QuilPragma pragma-p)
         (lisp ClQuilInstruction (pragma-p) pragma-p))
        ((QuilHalt halt-h)
         (lisp ClQuilInstruction (halt-h) halt-h)))))
  (define-instance (Iso QuilInstruction ClQuilInstruction)))

(coalton-toplevel

  (define-type QuilOperatorDescription
    (QuilNamedOperator QuilNamedOperator)
    (QuilDaggerOperator QuilDaggerOperator))

  (define-instance (into ClQuilOperatorDescription QuilOperatorDescription)
    (define (into operator-description-o)
      (lisp QuilOperatorDescription (operator-description-o)
        (adt:match cl-quil:operator-description operator-description-o
          ((cl-quil:named-operator)
           (coalton (QuilNamedOperator (lisp QuilNamedOperator () operator-description-o))))
          ((cl-quil:dagger-operator)
           (coalton (QuilDaggerOperator (lisp QuilDaggerOperator () operator-description-o))))
          (_ (coalton (error "Unexpected cl-quil:operator-description.

Must be in {cl-quil:named-operator, cl-quil:dagger-operator}.")))))))
  (define-instance (into QuilOperatorDescription ClQuilOperatorDescription)
    (define (into operator-description-o)
      (match operator-description-o
        ((QuilNamedOperator named-operator-o)
         (lisp ClQuilOperatorDescription (named-operator-o) named-operator-o))
        ((QuilDaggerOperator dagger-operator-o)
         (lisp ClQuilOperatorDescription (dagger-operator-o) dagger-operator-o)))))
  (define-instance (iso QuilOperatorDescription ClQuilOperatorDescription)))

(coalton-toplevel

  (define-type QuilExecutableCode (QuilExecutableCode (List QuilInstruction)))
  (define-instance (Into ClQuilExecutableCode QuilExecutableCode)
    (define (into cl-executable-code-c)
      (QuilExecutableCode
       (map into (lisp (List ClQuilInstruction) (cl-executable-code-c)
                   (cl:coerce cl-executable-code-c 'cl:list))))))
  (define-instance (Into QuilExecutableCode ClQuilExecutableCode)
    (define (into (QuilExecutableCode executable-code-c))
      (let ((cl-instructions (map (as ClQuilInstruction) executable-code-c)))
        (lisp ClQuilExecutableCode (cl-instructions)
          (cl:coerce cl-instructions 'cl:vector)))))
  (define-instance (Iso QuilExecutableCode ClQuilExecutableCode)))

(coalton-toplevel

  (declare parse-quil (String -> QuilParsedProgram))
  (define (parse-quil quil-string)
    "Parse Quil code, given as a `String`, into a `QuilParsedProgram`."
    (lisp QuilParsedProgram (quil-string)
      (cl-quil:parse-quil quil-string)))

  (declare parse-file ((Into :a coalton-library/file:Pathname) => :a -> QuilParsedProgram))
  (define (parse-file file)
    "Parse a file, such as a `.qasm` file, to a `QuilParsedProgram`."
    (let ((file-string (unwrap (coalton-library/file:read-file-to-string file))))
      (lisp QuilParsedProgram (file file-string)
        (cl-quil:parse file-string :originating-file file))))

  (declare parse-qasm (String -> QuilParsedProgram))
  (define (parse-qasm qasm-string)
    "Parse a QASM string to a `QuilParsedProgram`."
    (lisp QuilParsedProgram (qasm-string)
      (cl-quil.qasm:parse-qasm qasm-string))))

(coalton-toplevel

  (define-instance (Eq QuilParsedProgram)
    (define (== pp1 pp2)
      (lisp Boolean (pp1 pp2)
        (cl-quil::matrix-equals-dwim
         (cl-quil:parsed-program-to-logical-matrix pp1 :compress-qubits cl:t)
         (cl-quil:parsed-program-to-logical-matrix pp2 :compress-qubits cl:t)))))

  (declare copy-parsed-program (QuilParsedProgram -> QuilParsedProgram))
  (define (copy-parsed-program pp)
    "Copy a `QuilParsedProgram` object."
    (lisp QuilParsedProgram (pp)
      (cl-quil:copy-instance pp)))

  (declare print-parsed-program (QuilParsedProgram -> QuilParsedProgram))
  (define (print-parsed-program pp)
    "Print a `QuilParsedProgram`."
    (lisp QuilParsedProgram (pp)
      (cl:progn (cl-quil:print-parsed-program pp)
                pp)))

  (declare get-parsed-program-final-rewiring (QuilParsedProgram -> QuilRewiring))
  (define (get-parsed-program-final-rewiring pp)
    "Get the exit `QuilRewiring` from `pp`."
    (lisp QuilRewiring (pp)
      (cl:loop :for instr :across (cl-quil:parsed-program-executable-code pp)
         :do (cl:let ((exit (cl:nth-value 1 (cl-quil:instruction-rewirings instr))))
               (cl:if exit (cl:return exit))))))

  (declare get-parsed-program-memory-definitions (QuilParsedProgram -> (List QuilMemoryDescriptor)))
  (define (get-parsed-program-memory-definitions pp)
    "Get the memory definitions of `pp`."
    (lisp (List QuilMemoryDescriptor) (pp)
      (cl-quil:parsed-program-memory-definitions pp)))

  (declare set-parsed-program-memory-definitions! (QuilParsedProgram -> (List QuilMemoryDescriptor) -> QuilParsedProgram))
  (define (set-parsed-program-memory-definitions! pp ds)
    "Set the memory definitions of `pp` to `ds`."
    (lisp QuilParsedProgram (pp ds)
      (cl:progn (cl:setf (cl-quil:parsed-program-memory-definitions pp) ds)
                pp)))

  (declare map-parsed-program-memory-definitions! (((List QuilMemoryDescriptor) -> (List QuilMemoryDescriptor))
                                                   -> QuilParsedProgram -> QuilParsedProgram))
  (define (map-parsed-program-memory-definitions! f pp)
    "Map the memory definitions of `pp` over `f`."
    (pipe (f (get-parsed-program-memory-definitions pp))
          (set-parsed-program-memory-definitions! pp)))

  (declare get-parsed-program-executable-code (QuilParsedProgram -> QuilExecutableCode))
  (define (get-parsed-program-executable-code pp)
    "Get the `QuilExecutableCode` of `pp`."
    (into
     (lisp ClQuilExecutableCode (pp)
       (cl-quil:parsed-program-executable-code pp))))

  (declare set-parsed-program-executable-code! (QuilExecutableCode -> QuilParsedProgram -> QuilParsedProgram))
  (define (set-parsed-program-executable-code! executable-code-c pp)
    "Set the `QuilExecutableCode` of `pp`."
    (let ((cl-executable-code-c (as ClQuilExecutableCode executable-code-c)))
      (lisp QuilParsedProgram (pp cl-executable-code-c)
        (cl:progn (cl:setf (cl-quil:parsed-program-executable-code pp)
                           cl-executable-code-c)
                  pp))))

  (declare map-parsed-program-executable-code! ((QuilExecutableCode -> QuilExecutableCode)
                                                -> QuilParsedProgram -> QuilParsedProgram))
  (define (map-parsed-program-executable-code! f pp)
    "Map the `QuilExecutableCode` of `pp` over `f`."
    (pipe (f (get-parsed-program-executable-code pp))
          (flip set-parsed-program-executable-code! pp))))

(coalton-toplevel

  (declare get-quil-gate-application-qubits (QuilGateApplication -> (List UFix)))
  (define (get-quil-gate-application-qubits gate-application-g)
    "Get the `List` of qubits to which `gate-application-g` applies."
    (lisp (List UFix) (gate-application-g)
      (cl:map 'cl:list 'cl-quil:qubit-index (cl-quil:application-arguments gate-application-g))))

  (declare parsed-program-multi-qubit-depth (QuilParsedProgram -> UFix))
  (define (parsed-program-multi-qubit-depth parsed-program-p)
    "The number of gates applied to two or more qubits in a parsed program."
    (match (get-parsed-program-executable-code parsed-program-p)
      ((QuilExecutableCode instructions)
       (list:countby (fn (instruction)
                       (match instruction
                         ((QuilGateApplication gate-application-g)
                          (<= 2 (length (get-quil-gate-application-qubits gate-application-g))))
                         (_ False)))
                     instructions))))

  (declare get-quil-gate-application-angle (QuilGateApplication -> Fraction))
  (define (get-quil-gate-application-angle gate-application-g)
    "Unsafe! Get the angle which parameterizes `gate-application-g`; for rotation gates only.

The result will be a `Fraction` in [0,1) which corresponds to revolutions."
    (lisp Fraction (gate-application-g)
      (cl:rationalize
       (cl:/ (cl-quil:constant-value (cl:first (cl-quil:application-parameters gate-application-g))) 2 cl:pi)))))

(coalton-toplevel

  (declare get-quil-measure-qubit (QuilMeasure -> UFix))
  (define (get-quil-measure-qubit measure-m)
    "Get the qubit index measured by `measure-m`."
    (lisp UFix (measure-m)
      (cl-quil:qubit-index
       (cl-quil:measurement-qubit measure-m))))

  (declare get-quil-measure-discard-qubit (QuilMeasureDiscard -> UFix))
  (define (get-quil-measure-discard-qubit measure-discard-m)
    "Get the qubit index measured by `measure-discard-m`."
    (lisp UFix (measure-discard-m)
      (cl-quil:qubit-index
       (cl-quil:measurement-qubit measure-discard-m))))

  (declare get-quil-measurement-qubit (QuilMeasurement -> UFix))
  (define (get-quil-measurement-qubit measurement-m)
    "Get the qubit index measured by `measurement-m`."
    (match measurement-m
      ((QuilMeasure measure-m)
       (get-quil-measure-qubit measure-m))
      ((QuilMeasureDiscard measure-discard-m)
       (get-quil-measure-discard-qubit measure-discard-m))))

  (declare get-quil-measure-address (QuilMeasure -> QuilMemoryRef))
  (define (get-quil-measure-address measure-m)
    "Get the address to which `measure-m` writes a bit."
    (lisp QuilMemoryRef (measure-m)
      (cl-quil:measure-address measure-m)))

  (declare get-quil-measurement-address (QuilMeasurement -> (Optional QuilMemoryRef)))
  (define (get-quil-measurement-address measurement-m)
    "If `measurement-m` is a `QuilMeasure`, then get the address to which it writes a bit."
    (match measurement-m
      ((QuilMeasure measure-m)
       (Some (get-quil-measure-address measure-m)))
      ((QuilMeasureDiscard _)
       None))))

(coalton-toplevel

  (declare get-quil-instruction-qubits (QuilInstruction -> (List UFix)))
  (define (get-quil-instruction-qubits instruction)
    "Get a `List` of qubits included in a `QuilInstruction`."
    (match instruction
      ((QuilGateApplication gate-application)
       (get-quil-gate-application-qubits gate-application))
      ((QuilMeasurement measurement)
       (singleton (get-quil-measurement-qubit measurement)))
      (_ Nil)))

  (declare get-parsed-program-highest-qubit-index (QuilParsedProgram -> (Optional UFix)))
  (define (get-parsed-program-highest-qubit-index pp)
    "Get the highest qubit index included in `pp`. Returns `None` if `pp` does not contain qubit instructions."
    (match (get-parsed-program-executable-code pp)
      ((QuilExecutableCode instructions)
       (iter:max! (iter:flat-map! (compose iter:into-iter get-quil-instruction-qubits)
                                  (iter:into-iter instructions)))))))

(coalton-toplevel

  (declare get-quil-operator-description (QuilGateApplication -> QuilOperatorDescription))
  (define (get-quil-operator-description gate-application-g)
    "Get the `operator-description` of a `gate-application`."
    (lisp QuilOperatorDescription (gate-application-g)
      (cl:let ((operator-description-o (cl-quil:application-operator gate-application-g)))
        (adt:match cl-quil:operator-description operator-description-o
          ((cl-quil:named-operator)
           (coalton (QuilNamedOperator (lisp QuilNamedOperator () operator-description-o))))
          ((cl-quil:dagger-operator)
           (coalton (QuilDaggerOperator (lisp QuilDaggerOperator () operator-description-o))))
          (_ (coalton (error "Unexpected cl-quil:operator-description.

Must be in {cl-quil:named-operator, cl-quil:dagger-operator}.")))))))

  (declare get-quil-named-operator-name (QuilNamedOperator -> String))
  (define (get-quil-named-operator-name named-operator-o)
    "Get the name of `named-operator-o`."
    (lisp String (named-operator-o)
      (adt:match cl-quil:operator-description named-operator-o
        ((cl-quil:named-operator s) s)
        (_ (coalton (error "QuilNamedOperator does not have a name."))))))

  (declare get-quil-dagger-operator-operator (QuilDaggerOperator -> QuilOperatorDescription))
  (define (get-quil-dagger-operator-operator dagger-operator-o)
    "Get the `operator-description` which is modified by `dagger-operator`."
    (lisp QuilOperatorDescription (dagger-operator-o)
      (adt:match cl-quil:operator-description dagger-operator-o
        ((cl-quil:dagger-operator operator-description-o)
         (adt:match cl-quil:operator-description operator-description-o
           ((cl-quil:named-operator)
            (coalton (QuilNamedOperator (lisp QuilNamedOperator () operator-description-o))))
           ((cl-quil:dagger-operator)
            (coalton (QuilDaggerOperator (lisp QuilDaggerOperator () operator-description-o))))
           (_ (coalton (error "Unexpected cl-quil:operator-description.

Must be in {cl-quil:named-operator, cl-quil:dagger-operator}.")))))
        (_ (coalton (error "Bad Operator")))))))

(coalton-toplevel

  (declare make-quil-gate-application (Boolean -> String -> (List Double-Float) -> (List UFix) -> QuilGateApplication))
  (define (make-quil-gate-application dag? name args qubits)
    "Make a `QuilGateApplication` from the supplied parameters."
    (if dag?
        (lisp QuilGateApplication (name args qubits)
          (cl:apply #'cl-quil:build-gate (cl-quil:dagger-operator (cl-quil:named-operator name)) args qubits))
        (lisp QuilGateApplication (name args qubits)
          (cl:apply #'cl-quil:build-gate name args qubits))))

  (declare make-quil-measurement ((Optional QuilMemoryRef) -> UFix -> QuilMeasurement))
  (define (make-quil-measurement wrapped-memory-ref index-q)
    "Make a `QuilMeasurement` from the supplied parameters, discarding if no address is supplied."
    (match wrapped-memory-ref
      ((Some memory-ref)
       (QuilMeasure
        (lisp QuilMeasure (memory-ref index-q)
          (cl:make-instance 'cl-quil:measure
                            :address memory-ref
                            :qubit (cl-quil:qubit index-q)))))
      ((None)
       (QuilMeasureDiscard
        (lisp QuilMeasureDiscard (index-q)
          (cl:make-instance 'cl-quil:measure-discard
                            :qubit (cl-quil:qubit index-q)))))))

  (declare make-quil-classical-move (QuilMemoryRef -> UFix -> QuilClassicalMove))
  (define (make-quil-classical-move address bit)
    "Make a `QuilClassicalMove` from the supplied parameters."
    (lisp QuilClassicalMove (address bit)
      (cl:make-instance 'cl-quil:classical-move-bit/immediate
                        :left address
                        :right (cl-quil:constant bit cl-quil:quil-bit))))

  (declare make-quil-classical-exclusive-or (QuilMemoryRef -> QuilMemoryRef -> QuilClassicalExclusiveOr))
  (define (make-quil-classical-exclusive-or left-address right-address)
    "Make a `QuilClassicalExclusiveOr` from the supplied parameters."
    (lisp QuilClassicalExclusiveOr (left-address right-address)
      (cl:make-instance 'cl-quil:classical-exclusive-or-bit/bit
                        :left left-address
                        :right right-address)))

  (declare make-quil-memory-descriptor (String -> UFix -> QuilMemoryDescriptor))
  (define (make-quil-memory-descriptor name size)
    "Make a QuilMemoryDescriptor called `name` of size `size`. The descriptor will be of type `cl-quil:quil-bit`."
    (lisp QuilMemoryDescriptor (name size)
      (cl-quil:make-memory-descriptor :name name
                                      :type cl-quil:quil-bit
                                      :length size)))

  (declare make-quil-memory-ref (QuilMemoryDescriptor -> UFix -> QuilMemoryRef))
  (define (make-quil-memory-ref memory-ref-r idx)
    "Make a new `QuilMemoryRef` for `idx` in register `memory-ref-r`."
    (lisp QuilMemoryRef (memory-ref-r idx)
      (cl-quil:mref (cl-quil:memory-descriptor-name memory-ref-r) idx memory-ref-r))))

(coalton-toplevel

  (declare compiler-hook (QuilParsedProgram -> QuilChipSpecification -> Boolean -> Boolean
                                            -> (Tuple3 QuilParsedProgram UFix Fraction)))
  (define (compiler-hook parsed-program chip-specification protoquil? destructive?)
    "Compile `parsed-program` to the given `chip-specification`."
    (lisp (Tuple3 QuilParsedProgram UFix Fraction) (parsed-program chip-specification protoquil? destructive?)
      (cl:let ((cl-user:*muffled-warnings* cl:t))
        (cl:multiple-value-bind (cpp swaps duration)
            (cl-quil:compiler-hook parsed-program chip-specification
                                   :protoquil protoquil? :destructive destructive?)
          (Tuple3 cpp swaps duration)))))

  (declare nativize-executable-code (QuilChipSpecification -> QuilExecutableCode -> QuilExecutableCode))
  (define (nativize-executable-code chip-specification executable-code)
    "Compile `executable-code` to the restrictions of `chip-specification` without applying optimizations."
    (let ((cl-executable-code (as ClQuilExecutableCode executable-code)))
      (as QuilExecutableCode
       (lisp ClQuilExecutableCode (chip-specification cl-executable-code)
         (cl:coerce
          (cl-quil::expand-to-native-instructions
           (cl:coerce cl-executable-code 'cl:list)
           chip-specification)
          'cl:simple-vector))))))

;; This macro is copied directly from
;; quilc/benchmarking/rewiring-analysis.lisp
;; written by Robert Smith.
(cl:defmacro with-stopwatch (elapsed-var cl:&body body)
  (cl:let ((start-time (cl:gensym)))
    `(cl:let ((,start-time (cl:get-internal-real-time)))
       (cl:symbol-macrolet ((,elapsed-var (cl:- (cl:get-internal-real-time) ,start-time)))
         ,@body))))

;; The code in the following block is adapted from
;; the function benchmark-qasm-suite in the file
;; quilc/benchmarking/qasm-benchmarking.lisp
;; for use with Foust in Coalton.
(coalton-toplevel

  (declare parsed-program-metrics (UFix -> QuilChipSpecification -> (QuilParsedProgram -> QuilParsedProgram) -> QuilParsedProgram
                                        -> (Optional (Tuple3 Double-Float UFix UFix))))
  (define (parsed-program-metrics timeout chip pre-operation parsed-program)
    "Compile `parsed-program` to `chip` and return the elapsed time (s), number of topological swaps, and multi-qubit depth."
    (lisp (Optional (Tuple3 Double-Float UFix UFix)) (timeout chip pre-operation parsed-program)
      (trivial-garbage:gc :full cl:t)
      (cl:handler-case
          (bordeaux-threads:with-timeout (timeout)
            (with-stopwatch elapsed-time
              (coalton
               (match (compiler-hook ((lisp (QuilParsedProgram -> QuilParsedProgram) () pre-operation)
                                      (lisp QuilParsedProgram () parsed-program))
                                     (lisp QuilChipSpecification () chip)
                                     False
                                     False)
                 ((Tuple3 compiled-parsed-program swaps _)
                  (Some (Tuple3 (lisp Double-Float () (cl:coerce (cl:/ elapsed-time 1000000) 'cl:double-float))
                                swaps
                                (parsed-program-multi-qubit-depth compiled-parsed-program))))))))
        (bordeaux-threads:timeout () None)))))

(coalton-toplevel

  (declare map-from-links ((List (List UFix)) -> (map:Map UFix (tree:Tree UFix))))
  (define (map-from-links links)
    "Construct a `Map` representing the graph given by a list of edges called `links`."
    (let ((expanded-links (concatmap
                           (fn (link)
                             (match link
                               ((Cons qi (Cons qj (Nil)))
                                (make-list (Tuple qi qj) (Tuple qj qi)))
                               (_ (error "Unnexpected link."))))
                           links)))
      (fold (fn (link-map (Tuple from to))
              (unwrap (map:update (flip tree:insert-or-replace to) link-map from)))
            (map:collect (map (compose (pair-with (const tree:Empty)) fst) expanded-links))
            expanded-links)))

  (declare get-chip-specification-links (QuilChipSpecification -> (map:Map UFix (tree:Tree UFix))))
  (define (get-chip-specification-links chip-spec)
    "Construct a `Map` from qubit indices to `Tree`s of qubit indices, representing the connectivity from a `QuilChipSpecification`."
    (map-from-links
     (lisp (List (List UFix)) (chip-spec)
       (cl:map 'cl:list
               (cl:lambda (link)
                 (cl:coerce (cl-quil::vnth 0 (cl-quil::hardware-object-cxns link)) 'cl:list))
               (cl-quil::chip-spec-links chip-spec)))))

  (declare build-IBM-Qx5 (Unit -> QuilChipSpecification))
  (define (build-IBM-Qx5)
    "Construct a `QuilChipSpecification` corresponding to the specifications of the IBM Qx5 chip."
    (lisp QuilChipSpecification () (cl-quil::build-IBM-Qx5)))

  (declare build-nQ-fully-connected-chip (UFix -> (List String) -> QuilChipSpecification))
  (define (build-nQ-fully-connected-chip n architecture)
    "Construct a `QuilChipSpecification` for a chip with fully connected qubits with the architecture specified.

Example usage:

    (build-nQ-fully-connected-chip 5 (make-list \":CZ\" \":CNOT\"))"
    (lisp QuilChipSpecification (n architecture)
      (cl-quil::build-nQ-fully-connected-chip
       n
       :architecture (cl:map 'cl:list #'cl:read-from-string architecture)))))

(cl:defun make-gate-field (operator cl:&key parameters arguments qubit target)
  (cl:let ((gate-hash (cl:make-hash-table :test #'cl:equalp)))
    (cl:setf (cl:gethash "operator" gate-hash) operator)
    (cl:cond
      (arguments
       (cl:progn
         (cl:setf (cl:gethash "parameters" gate-hash) parameters)
         (cl:setf (cl:gethash "arguments" gate-hash) arguments)))
      (qubit
       (cl:progn
         (cl:setf (cl:gethash "qubit" gate-hash) qubit)
         (cl:if target (cl:setf (cl:gethash "target" gate-hash) target))))
      (cl:t (cl:error "make-gate-field missing arguments or qubit field.")))
    gate-hash))

(coalton-toplevel

  (declare build-nQ-foust-chip (UFix -> QuilChipSpecification))
  (define (build-nQ-foust-chip number-of-qubits)
    "Construct a `QuilChipSpecification` to compile instructions to the gate set supported by Foust."
    (lisp QuilChipSpecification (number-of-qubits)
      (cl:let ((chip-spec (cl-quil::make-chip-specification
                           :generic-rewriting-rules (cl:coerce (cl-quil::global-rewriting-rules) 'cl:vector)))
               (single-qubit-gate-information (cl-quil::parse-gates-field
                                               (cl:list
                                                (make-gate-field "X" :arguments (cl:list "_"))
                                                (make-gate-field "Y" :arguments (cl:list "_"))
                                                (make-gate-field "Z" :arguments (cl:list "_"))
                                                (make-gate-field "H" :arguments (cl:list "_"))
                                                (make-gate-field "S" :arguments (cl:list "_"))
                                                (make-gate-field "T" :arguments (cl:list "_"))
                                                (make-gate-field "RX" :parameters (cl:list "_") :arguments (cl:list "_"))
                                                (make-gate-field "RY" :parameters (cl:list "_") :arguments (cl:list "_"))
                                                (make-gate-field "RZ" :parameters (cl:list "_") :arguments (cl:list "_"))
                                                (make-gate-field "MEASURE" :qubit "_")
                                                (make-gate-field "MEASURE" :qubit "_" :target "_"))))
               (two-qubit-gate-information (cl-quil::parse-gates-field
                                            (cl:list
                                             (make-gate-field "CNOT" :arguments (cl:list "_" "_"))
                                             (make-gate-field "CZ" :arguments (cl:list "_" "_"))
                                             (make-gate-field "ISWAP" :arguments (cl:list "_" "_"))
                                             (make-gate-field "SWAP" :arguments (cl:list "_" "_"))))))
        (cl-quil::install-generic-compilers chip-spec '(:cnot :cz :iswap :swap))
        (cl:dotimes (q number-of-qubits)
          (cl-quil::adjoin-hardware-object (cl-quil::build-qubit q :gate-information single-qubit-gate-information) chip-spec))
        (cl:dotimes (q-two number-of-qubits)
          (cl:dotimes (q-one q-two)
            (cl:let ((link (cl-quil::build-link q-one q-two :gate-information two-qubit-gate-information))
                     (link-index (cl-quil::chip-spec-n-links chip-spec)))
              (cl-quil::adjoin-hardware-object link chip-spec)
              (cl:vector-push-extend link-index (cl-quil::vnth 1 (cl-quil::hardware-object-cxns
                                                                  (cl-quil::chip-spec-nth-qubit chip-spec q-one))))
              (cl:vector-push-extend link-index (cl-quil::vnth 1 (cl-quil::hardware-object-cxns
                                                                  (cl-quil::chip-spec-nth-qubit chip-spec q-two)))))))
        (cl-quil::warm-hardware-objects chip-spec)))))

(coalton-toplevel

  (declare get-quil-rewiring-l2p (QuilRewiring -> (List UFix)))
  (define (get-quil-rewiring-l2p rewiring)
    "Get the logical-to-physical qubit map from `rewiring`."
    (lisp (List UFix) (rewiring)
      (cl:coerce (cl-quil:rewiring-l2p rewiring) 'cl:list)))

  (declare get-quil-rewiring-p2l (QuilRewiring -> (List UFix)))
  (define (get-quil-rewiring-p2l rewiring)
    "Get the physical-to-logical qubit map from `rewiring`."
    (lisp (List UFix) (rewiring)
      (cl:coerce (cl-quil:rewiring-p2l rewiring) 'cl:list))))
