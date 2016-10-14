;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

;;;; packages for cl-web-crawler

(defpackage #:unique-queue
  (:use :cl)
  (:nicknames :uniqq)
  (:export
   #:make-unique-queue
   #:q-add
   #:q-pop
   #:q-empty
   #:q-existed
   ))

(defpackage #:web-crawler
  (:use :cl :drakma :puri :cl-ppcre :html-parse :unique-queue)
  (:documentation "Main package for web-crawler.")
  (:nicknames :webc)
  (:export
   #:make-same-host-filter
   #:start-crawl))

