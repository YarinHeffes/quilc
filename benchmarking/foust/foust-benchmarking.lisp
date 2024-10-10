;;;; Author: Yarin Heffes

(defpackage #:cl-quil-benchmarking.foust
  (:use
   #:coalton
   #:coalton-prelude)
  (:use
   #:coalton-quil
   #:cl-quil.foust-quil)
  (:import-from #:coalton-library/math/complex #:square-magnitude)
  (:local-nicknames
   (#:bits #:coalton-library/bits)
   (#:cell #:coalton-library/cell)
   (#:file #:coalton-library/file)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:map #:coalton-library/ord-map)
   (#:string #:coalton-library/string))
  (:export
   #:foust-benchmark-qasm-suite
   #:cl-foust-benchmark-qasm-suite))

(in-package #:cl-quil-benchmarking.foust)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare qasm-prefix file:Pathname)
  (define qasm-prefix
    "The pathname of the directory in which the QASM benchmarking files are stored."
    (unwrap (file:system-relative-pathname "cl-quil" "benchmarking/ibm_qx_mapping/examples/")))

  (declare qasm-test-files (Unit -> (List file:Pathname)))
  (define (qasm-test-files)
    "A list of pathnames associated with the QASM benchmarking files."
    (filter (compose (string:substring? ".qasm") into) (unwrap (file:directory-files qasm-prefix))))

  (declare nstring (UFix -> String -> String))
  (define (nstring n str)
    "Repeat the String `str` `n` times and concatenate."
    (iter:fold! <> mempty (iter:take! n (iter:repeat str))))

  (declare num->stringm ((Num :a) (Into :a String) => UFix -> :a -> String))
  (define (num->stringm m n)
    "Produce a String of length `m` from the String representation of the number `n`."
    (let ((str (string:substring (into n) 0 m))
          (len (string:length str)))
      (string:concat (nstring (- m len) " ") str)))

  (declare stringm (UFix -> String -> String))
  (define (stringm m str)
    "Produce a String of length `m` from `str`, adding trailing spaces or trimming as needed."
    (let ((str2 (string:substring str 0 16)))
      (string:concat str2 (nstring (- m (string:length str2)) " "))))

  (declare qasm-measurements String)
  (define qasm-measurements
    (lisp String ()
      (cl:format cl:nil "~{measure q[~a] -> c[~a];~%~}"
                 (cl:apply #'cl:nconc (cl:loop for n from 0 below 16 collect (cl:list n n))))))

  (declare parsed-program-metrics-as-string ((Optional (Tuple3 Double-Float UFix UFix)) -> String))
  (define (parsed-program-metrics-as-string metrics)
    "Produce a string representation of the metrics of a parsed program."
    (match metrics
      ((Some (Tuple3 elapsed-time swaps depth))
       (mconcat (make-list (num->stringm 8 elapsed-time) "  " (num->stringm 5 swaps) "  " (num->stringm 8 depth))))
      ((None) "TIMEOUT!  ?????  ????????")))

  (declare foust-benchmark-qasm-suite (UFix -> Unit))
  (define (foust-benchmark-qasm-suite timeout)
    "Benchmark Foust by compiling a suite of QASM files to the chip ibm-qx-5 with the Quil compiler, with and without using Foust in its preserve mode."
    (print "┌──────────────────┬───────────────────────────┬───────────────────────────┬───────────────────────────┐")
    (print "│                  │       WITHOUT FOUST       │   WITH PRESERVING FOUST   │    WITH RELEASING FOUST   │")
    (print "├──────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┤")
    (print "│       NAME       │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │ TIME (s)  SWAPS  2Q DEPTH │")
    (print "├──────────────────┼───────────────────────────┼───────────────────────────┼───────────────────────────┤")
    (for file-f in (qasm-test-files)
      (let ((file-name (pipe file-f
                             into
                             (compose unwrap (string:strip-prefix (into qasm-prefix)))
                             (compose unwrap (string:strip-suffix ".qasm"))
                             (stringm 16)))
            (parsed-program (parse-qasm (<> (unwrap (file:read-file-to-string file-f)) qasm-measurements))))
        (lisp Boolean (file-name) (cl:format cl:t "│ ~A │ " file-name))
        (for pre-operation in (make-list id
                                         (fn (pp) (foust-parsed-program pp None True False))
                                         (fn (pp) (foust-parsed-program pp None False False)))
          (let ((metrics (parsed-program-metrics-as-string
                          (parsed-program-metrics timeout (build-IBM-Qx5) pre-operation parsed-program))))
            (lisp Boolean (metrics) (cl:format cl:t "~A │ " metrics))))
        (print "")))
    (print "└──────────────────┴───────────────────────────┴───────────────────────────┴───────────────────────────┘")))

(cl:defun cl-foust-benchmark-qasm-suite (cl:&key (timeout 30))
  (coalton (foust-benchmark-qasm-suite (lisp UFix () timeout))))
