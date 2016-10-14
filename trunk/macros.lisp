;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

(in-package :web-crawler)


(defmacro cut (&body body)
  (let ((syms nil))
    (labels ((gen-used-syms (tree)
               (mapcar #'(lambda (elem)
                           (typecase elem
                             (symbol
                              (if (string= (symbol-name elem) "_")
                                  (let ((new (gensym "ARG")))
                                    (push new syms)
                                    new)
                                  elem))
                             (list
                              (gen-used-syms elem))
                             (t elem)))
                       tree)))
      (let ((newbody (gen-used-syms body)))
        `(function (lambda ,(nreverse syms)
             ,newbody))))))


; dynamic function binding
;(defmacro letf (bindings &body body)
;  (loop for



