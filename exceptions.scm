;;; rnrs exceptions (6) --- R6RS exceptions

;; Copyright (C) 2013  Taylan Ulrich Bayırlı/Kammer

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: ffi struct bytevector bytestructure

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A clean implementation of (rnrs exceptions (6)).  The dynamic environment
;; capturing operations are noteworthy.


;;; Code:

(library
 (rnrs exceptions (6))
 (export with-exception-handler raise raise-continuable guard)
 (import (rnrs base (6))
         (srfi :39))

;;; Helpers

;;; Ignores any extra `else' clauses.
;;; Helps to generate cond clauses with a default `else' clause.
 (define-syntax cond+
   (syntax-rules (else)
     ((cond+ clause ... (else else1) (else else2))
      (cond+ clause ... (else else1)))
     ((cond+ clause ...)
      (cond clause ...))))

;;; Captures the current dynamic environment.  It is reified as a procedure that
;;; accepts a thunk and executes it in the captured dynenv.
 (define (capture-dynenv)
   ((call/cc
     (lambda (captured-env)
       (lambda ()
         (lambda (proc)
           (call/cc
            (lambda (go-back)
              (captured-env
               (lambda ()
                 (call-with-values proc go-back)))))))))))

;;; Captures the current dynamic environment and returns a procedure that
;;; accepts as many arguments as PROC and applies PROC to them in that dynenv.
;;; In other words, returns a version of PROC that's tied to the current dynenv.
 (define (dynenv-proc proc)
   (let ((env (capture-dynenv)))
     (lambda args
       (env (lambda () (apply proc args))))))

;;; Returns a procedure that's always executed in the current dynamic
;;; environment and not the one from which it's called.
 (define-syntax dynenv-lambda
   (syntax-rules ()
     ((_ args body body* ...)
      (dynenv-proc (lambda args body body* ...)))))

 
;;; Main code:

 (define handlers (make-parameter '()))

 (define &non-continuable '&non-continuable)

 (define (with-exception-handler handler thunk)
   (parameterize ((handlers (cons handler (handlers))))
     (thunk)))

 (define (%raise condition continuable?)
   (if (null? (handlers))
       (error "unhandled exception" condition)
       (let ((handler (car (handlers))))
         (parameterize ((handlers (cdr (handlers))))
           (if continuable?
               (handler condition)
               (begin
                 (handler condition)
                 (%raise &non-continuable #f)))))))

 (define (raise-continuable condition)
   (%raise condition #t))

 (define (raise condition)
   (%raise condition #f))

 (define-syntax guard
   (syntax-rules ()
     ((guard (var clause clause* ...)
             body body* ...)
      (call/cc
       (lambda (return)
         (let ((handler (dynenv-lambda (var re-raise)
                          (return
                           (cond+ clause
                                  clause*
                                  ...
                                  (else (re-raise)))))))
           (with-exception-handler
            (lambda (condition)
              (let ((re-raise (dynenv-lambda ()
                                (raise condition))))
                (handler condition re-raise)))
            (lambda ()
              body body* ...))))))))
 )
