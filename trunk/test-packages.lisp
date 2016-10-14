;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

;;; Test package for web-crawler

(defpackage #:web-crawler-tests
  (:use :cl :fiveam :puri :web-crawler :uniqq :cl-interpol)
;  (:shadowing-import-from
;  web-crawler
;)
  )
