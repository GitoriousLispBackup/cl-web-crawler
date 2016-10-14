;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

(in-package :uniqq)

(defclass unique-queue ()
  ((qlist :initarg :qlist :accessor qlist)
   (qtail :initarg :qtail :accessor qtail)
   (key   :initarg :key :accessor key)
   (qhash :initarg :qhash :accessor qhash)))

(defun make-unique-queue (&key (test 'eql) (key #'identity))
  (make-instance 'unique-queue
                 :qlist '()
                 :qtail '()
                 :key key
                 :qhash (make-hash-table :test test)))

(defgeneric q-existed (queue item)
  (:method (q item)
    (gethash (funcall (key q) item) (qhash q))))

(defgeneric q-pop (queue)
  (:method (queue)
    (if (qlist queue)
      (values (pop (qlist queue)) t)
      (values nil nil))))

(defgeneric q-empty (queue)
  (:method (queue)
    (if (qlist queue) nil t)))

(defgeneric q-add (queue item)
  (:method (q item)
    (if (q-existed q item)
        nil
        (let ((new (list item)))
          (if (qlist q)
              (setf (cdr (qtail q)) new)
              (setf (qlist q) new))
          (setf (qtail q) new)
          (setf (gethash (funcall (key q) item) (qhash q)) t)
          t))))



