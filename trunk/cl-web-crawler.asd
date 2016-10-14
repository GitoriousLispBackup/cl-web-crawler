;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

(defpackage #:cl-web-crawler-asd
  (:use :cl :asdf))

(in-package :cl-web-crawler-asd)


(defsystem cl-web-crawler
  :name "Web-Crawler"
  :description "A generic web-crawling library for Common Lisp."
  :version "0.02"
  :author "Aaron Sokoloski <asokoloski@gmail.com>"
  :licence "MIT License"
  :depends-on (:drakma :puri :cl-ppcre :cl-html-parse)
  :components ((:file "packages")
               (:file "unique-queue" :depends-on ("packages"))
               (:file "conditions"   :depends-on ("packages"))
               (:file "macros"       :depends-on ("packages"))
               (:file "web-crawler"  :depends-on ("macros" "conditions")))

  :in-order-to ((test-op (load-op web-crawler-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :fiveam)
                             (intern "ALL"
                                     :web-crawler-tests)))
)


(defsystem web-crawler-tests
  :components ((:file "test-packages")
               (:file "tests" :depends-on ("test-packages")))
  :depends-on (:cl-web-crawler :fiveam :cl-interpol)
)



;; all test systems
(defmethod operation-done-p ((o test-op) (c system))
  (values nil))

(defmethod operation-done-p ((o test-op)
                             (c (eql (find-system 'web-crawler-tests))))
  (values nil))

