#lang scheme

(require scheme/foreign
         (rename-in scheme (-> ->/c)))

(provide define-pointer-type)

(unsafe!)

(define-syntax define-pointer-type
  (syntax-rules ()
    ((define-pointer-type name pred?)
     (define-values (name pred?)
       (let* ((tag (gensym 'name))
              (name (_cpointer tag))
              (pred? (lambda (obj) 
                       (and (cpointer? obj)
                            (cpointer-has-tag? obj tag)))))
         (values name pred?))))))

