;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

;;; CONDITIONS

(in-package :web-crawler)


(define-condition stop-crawling ()
  ((reason :initarg reason :reader reason)))


(defun skip-page (c)
  (invoke-restart 'skip-page))