;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

;;; Tests for web-crawler

(in-package :web-crawler-tests)

(enable-interpol-syntax)

(test cut-macro
  (is (equal 7 (funcall (cut - _ _) 10 3)))
  (is (equal 13 (funcall (cut + _ _) 10 3)))
  (is (equal 18
             (let ((x 5))
               (funcall (cut + _ _ x) 10 3))))
  (is (equal 5 (funcall (cut / (+ _ _) _) 9 11 4))))


(def-suite all
    :description "All web-crawler tests")

(in-suite all)

(test parse-robots-txt
  (is (equalp '(("*" "/cyberworld/map/" "/test/" "/test2")
                ("cybermapper"))
              (web-crawler::parse-robots-txt
"# test robots.txt file

User-agent: *
Disallow: /cyberworld/map/ # This is an infinite virtual URL space
Disallow: /test/
Disallow: /test2

# Cybermapper knows where to go.
User-agent: cybermapper
Disallow:

")
              )))

(test split-on-newlines
  (is (equalp '("hello" "there" "fool")
              (web-crawler::split-on-newlines #?"hello\nthere\nfool")))
  (is (equalp '("hello" "there" "")
              (web-crawler::split-on-newlines #?"hello\nthere\n"))))


(test string-only-whitespace-p
  (is (web-crawler::string-only-whitespace-p ""))
  (is (web-crawler::string-only-whitespace-p " "))
  (is (web-crawler::string-only-whitespace-p " 		"))
  (is (web-crawler::string-only-whitespace-p #?"\r\n"))
  (is (web-crawler::string-only-whitespace-p #?"\t \r\n"))
  (is (not (web-crawler::string-only-whitespace-p "hello")))
  (is (not (web-crawler::string-only-whitespace-p "h    ")))
  (is (not (web-crawler::string-only-whitespace-p "    o")))
  (is (not (web-crawler::string-only-whitespace-p #?"\ro")))
  (is (not (web-crawler::string-only-whitespace-p #?"\no\n")))
  (is (not (web-crawler::string-only-whitespace-p "   a\t\t")))
  (is (not (web-crawler::string-only-whitespace-p "\t\te\t"))))


(test remove-comment
  (is (string= "hi there " (web-crawler::remove-comment "hi there # you mangy # dog")))
  (is (string= "" (web-crawler::remove-comment "# hi there you mangy dog")))
  (is (string= "hello" (web-crawler::remove-comment "hello"))))



(defparameter *fake-uri-list*
  (mapcar #'uri '("http://mises.org/foo"
                  "http://mises.org/bar"
                  "http://mises.org/baz"
                  "http://mises.org/quux")))

(test unique-queue
  (let ((q (make-unique-queue)))
    (is (q-empty q))
    (is (q-add q 'foo))
    (is (q-existed q 'foo))
    (is (not (q-empty q)))
    (is (not (q-add q 'foo)))
    (is (q-existed q 'foo))
    (multiple-value-bind (val was-val) (q-pop q)
      (is (eql val 'foo))
      (is (eql was-val t)))
    (is (q-empty q))
    (multiple-value-bind (val was-val) (q-pop q)
      (is (eql val nil))
      (is (eql was-val nil)))
    (is (q-existed q 'foo))
    (is (q-empty q))
    (is (q-add q 'bar))
    (is (not (q-empty q)))
    (multiple-value-bind (val was-val) (q-pop q)
      (is (eql val 'bar))
      (is (eql was-val t)))
    (is (not (q-add q 'bar)))
    (multiple-value-bind (val was-val) (q-pop q)
      (is (eql val nil))
      (is (eql was-val nil)))
    (is (q-empty q)))
  (let ((q (make-unique-queue :test 'equalp :key #'(lambda (u) (puri:render-uri u nil)))))
    (is (q-add q (puri:uri "http://foo")))
    (is (not (q-add q (puri:uri "http://foo"))))
    (is (q-existed q (puri:uri "http://foo")))))




(defclass mock-object ()
  ())

(defmethod webc::get-page ((fake-uri mock-object))
  (values (make-instance 'mock-object)) fake-uri)

(defmethod webc::find-all-links ((fake-text mock-object) &optional uri)
  (format t "finding links in ~S, base is ~S" fake-text uri))


;(test web-crawler
;  (start-crawl (make-instance 'mock-object) (lambda (a b) (print a)) :crawl-delay 1 :verbose t)
;  (is (eq t t)))





(disable-interpol-syntax)