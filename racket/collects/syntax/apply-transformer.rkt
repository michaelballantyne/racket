#lang racket/base

(require racket/syntax)

(provide local-apply-transformer)

(define (local-apply-transformer transformer stx context [intdef-ctxs '()])
  (unless (or (set!-transformer? transformer)
              (and (procedure? transformer)
                   (procedure-arity-includes? transformer 1)))
    (raise-argument-error 'local-apply-transformer
                          "(or/c (-> syntax? syntax?) set!-transformer?)"
                          transformer))
  (unless (syntax? stx)
    (raise-argument-error 'local-apply-transformer "syntax?" stx))
  (unless (or (eq? context 'expression)
              (eq? context 'top-level)
              (eq? context 'module)
              (eq? context 'module-begin)
              (list? context))
    (raise-argument-error 'local-apply-transformer
                          "(or/c 'expression 'top-level 'module 'module-begin list?)"
                          context))
  (unless (and (list? intdef-ctxs)
               (andmap internal-definition-context? intdef-ctxs))
    (raise-argument-error 'local-apply-transformer
                          "(listof internal-definition-context?)"
                          intdef-ctxs))
  (unless (syntax-transforming?)
    (raise-arguments-error 'local-apply-transformer "not currently expanding"))

  (let ([transformer-proc (if (set!-transformer? transformer)
                              (set!-transformer-procedure transformer)
                              transformer)])
    (syntax-local-apply-transformer transformer-proc
                                    (generate-temporary 'local-apply-transformer)
                                    context
                                    (cond
                                      [(null? intdef-ctxs) #f]
                                      [(= (length intdef-ctxs) 1) (car intdef-ctxs)]
                                      [else (error 'local-apply-transformer "only one intdef supported now")])
                                    stx)))
