;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; See file COPYING for license info

;;; Main code for web-crawler

(in-package :web-crawler)

(defun split-on-element (seq el)
  "Split any sequence into a list, using the given element
   as a separator."
  (let ((start 0)
        (segments nil))
    (loop for x = (position el seq :start start)
          while x
          do (push (subseq seq start x) segments)
          do (setq start (1+ x)))
    (nreverse (cons (subseq seq start) segments))))

(defun split-on-newlines (text)
  (split-on-element (remove #\Return text) #\Newline))

(defun string-only-whitespace-p (text)
  (every (cut member _ '(#\Space #\Tab #\Return #\Newline)) text))

(defun remove-comment (line)
  (subseq line 0 (position #\# line)))

(defun parse-robots-txt (text)
  "Parses the text of a robots.txt file and gives back
   a list of rules, each of the form
   (user-agent disallow-path*), where user-agent is a string,
   possibly \"*\".  An example rule is
   (\"*\" \"/cyberworld/map/\" \"/test/\" \"/test2\")"
  (let ((lines (split-on-newlines text))
        (rules nil)
        (ua nil))
    (loop for line in lines
          do (ppcre:register-groups-bind (field value)
                 ("^(.*?):\\s*(\\S+)\\s*" (remove-comment line))
               (cond
                 ((string-equal field "user-agent")
                  (setq ua value)
                  (let ((u (assoc ua rules)))
                    (when (null u)
                      (setq rules (cons (list ua) rules)))))
                 ((string-equal field "disallow")
                  (let ((rule (assoc ua rules)))
                    (setf (cdr rule) (push value (cdr rule))))))))
    ;; reverse the disallow order and the rule order
    (mapcar #'(lambda (x)
                (cons (car x) (nreverse (cdr x))))
            (nreverse rules))))

(defparameter *site-robots-rules* (make-hash-table :test 'equal))
(defparameter *user-agent* "whatever, not used yet")

(defun root-of-uri (uri)
  (let ((new (copy-uri uri)))
    (setf (uri-path new) "")
    new))

(defun get-robots-uri (uri)
  (let ((robots-uri (copy-uri (uri uri))))
    (setf (uri-path robots-uri) "/robots.txt")
    (render-uri robots-uri nil)))

(defun get-robots-txt (uri)
  (drakma:http-request uri))

(defun get-robots-rules (uri)
  "Gets the robots.txt rules for a url as returned by parse-robots-txt.
Caches when possible."
  (let ((robot-uri (get-robots-uri uri)))
    (or (gethash robot-uri *site-robots-rules*)
        (let ((rules (parse-robots-txt (get-robots-txt robot-uri))))
          (setf (gethash robot-uri *site-robots-rules*) rules)
          rules))))

(defun starts-with (pre seq)
  (let ((prelen (length pre)))
    (and (<= prelen (length seq))
         (search pre seq :end2 prelen))))

(defun uri-is-allowed (uri)
  "Check the robots.txt file for the site of this url, and
return whether the url is allowed for robots or not."
  (let* ((norm (uri uri))
         (path (uri-path norm))
         (rules (get-robots-rules norm))
         (rule (or (assoc *user-agent* rules :test #'string-equal)
                   (assoc "*" rules :test #'string-equal))))
    (if rule
        (notany (cut starts-with _ path) (cdr rule))
        t)))


;; with thanks to HTML::Tagset, http://search.cpan.org/~petdance/HTML-Tagset-3.10/
;(defparameter *link-elements*
;  '(
;    (:a          :href)
;    (:applet     :archive :codebase :code)
;    (:area       :href)
;    (:base       :href)
;    (:bgsound    :src)
;    (:blockquote :cite)
;    (:body       :background)
;    (:del        :cite)
;    (:embed      :pluginspage :src)
;    (:form       :action)
;    (:frame      :src :longdesc)
;    (:iframe     :src :longdesc)
;    (:ilayer     :background)
;    (:img        :src :lowsrc :longdesc :usemap)
;    (:input      :src :usemap)
;    (:ins        :cite)
;    (:isindex    :action)
;    (:head       :profile)
;    (:layer      :background :src)
;    (:link       :href)
;    (:object     :classid :codebase :data :archive :usemap)
;    (:q          :cite)
;    (:script     :src :for)
;    (:table      :background)
;    (:td         :background)
;    (:th         :background)
;    (:tr         :background)
;    (:xmp        :href)
;    ))

(defparameter *useful-link-elements*
  '(
    (:a          :href)
    (:area       :href)
    (:blockquote :cite)
    (:del        :cite)
    (:frame      :src)
    (:iframe     :src)
    (:ins        :cite)
    (:layer      :src)
    (:link       :href)
    (:q          :cite)
    (:xmp        :href)
    ))

(defgeneric find-all-links (text &optional base)
  (:method (html-text &optional base)
    (let* ((links nil)
           (base-callback (cons :base #'(lambda (elem)
                                          (print "FOUND BASE TAG")
                                          (let ((b (get-attr :href elem)))
                                            (when b (setq base b))))))
           (element-callbacks
            (mapcar (lambda (elem)
                      (destructuring-bind (tag attribute) elem
                        (cons tag
                              (cut push (get-attr attribute _) links))))
                    *useful-link-elements*)))
      (parse-html html-text
                  :callback-only t
                  :callbacks (cons base-callback element-callbacks))
      (mapcan
       #'(lambda (uri)
           (when uri
             (handler-case (list (merge-uris uri base))
               (uri-parse-error () nil))))
       links))))

(defun get-attr (attr elem)
  (when (consp (car elem))
    (loop for x on (cdar elem) by #'cddr
          when (eq attr (first x))
          return (second x))))


(defun is-success (status-code)
  (= status-code 200))


(defgeneric get-page (uri)
  (:documentation   "Do an HTTP GET request on a URI, closing the stream when the
request is done.  Returns
  (values <page content> <actual uri returned>)
The returned uri may be different due to a redirect.")
  (:method ((uri uri))
           (multiple-value-bind (body status headers actual-uri stream must-close reason-phrase)
               (http-request uri :method :get)
             (declare (ignore headers reason-phrase))
             (when must-close (close stream))
             (when (is-success status)
               (values body actual-uri)))))


;;;; EXPORTED FUNCTIONS

(defun make-same-host-filter (uri)
  "Given a either a string uri or a puri:uri object,
returns a function that takes one uri as an argument and returns
true if that uri has the same hostname as the original uri."
  (let ((host (uri-host (uri uri))))
    (lambda (test)
      (when (string-equal host (uri-host (uri test)))
        t))))

(defun make-save-page-processor (&optional (dir *default-pathname-defaults*))
  (let ((index-file (merge-pathnames "index.dat" dir))
        (counter 0))
    (lambda (url content)
      (let ((filename (format nil "file-~4,'0D.save" counter)))
        (with-open-file (OUT (merge-pathnames filename dir) :direction :output :if-exists :error)
          (write-string content OUT)
          (with-open-file (INDEX index-file
                                 :direction :output
                                 :if-exists (if (zerop counter) :error :append)
                                 :if-does-not-exist :create)
            (format INDEX "~A ~A~%" filename url)
            (incf counter)))))))

(defun crawl-and-save-site (start-uri dir &key (crawl-delay 10) verbose)
  "Crawl a site (one hostname), starting at START-URI, and save every page
to a file in DIR.  The file index.dat will contain a list of filenames and
their uris, because uris don't work so well as filenames."
  (start-crawl start-uri
               (make-save-page-processor dir)
               :uri-filter (make-same-host-filter start-uri)
               :crawl-delay crawl-delay
               :verbose verbose))

(defun start-crawl (uri processor &key uri-filter (crawl-delay 10) verbose)
  "Crawl the web, starting at <uri>.

<processor> must be a function that takes three arguments, the current url,
the parent of the current url and the content returned from doing a GET request on that url.  
The return value is ignored.

<:uri-filter> can be a predicate that should return false if the passed
  url should not be processed.  The url will be an instance of the
  puri:url class, which should be easier to deal with than plain text.
  If you want plain text, call (puri:render-uri url nil), which will
  return it.
  To limit crawling to one site, call (make-same-host-filter uri) and pass
  the return value in for :uri-filter.

<:crawl-delay> is a number of seconds to sleep between requests.  The
  default is 10.  It can be a fraction.

<:verbose> when true, prints out each uri being processed."
  (let ((queue (make-unique-queue :test 'equalp
                                  :key #'(lambda (u) (puri:render-uri (first u) nil)))))
    (labels ((crawl-page (uri)
               (multiple-value-bind (text real-uri) (get-page uri)
                 (let ((links (find-all-links text real-uri)))
                   (loop for link in links
                         do (setf (uri-fragment link) nil) ; get rid of links to anchors
                         do (when
                                (and (not (q-existed queue (list link real-uri)))
                                     (or (not uri-filter)
                                         (funcall uri-filter link)))
                               (q-add queue (list link real-uri)))))
                 (values text real-uri))))
      ;; START: with just the passed url
      (q-add queue (list (uri uri) nil))
      (loop until (q-empty queue)
            do
            (let* ((link-data (q-pop queue))
                   (parent-url (second link-data))
                   (next (first link-data)))
               (when (uri-is-allowed next)
                 (restart-case
                     (multiple-value-bind (page-text real-uri) (crawl-page next)
                       (when verbose
                         (format t "processing ~S~%" (render-uri real-uri nil)))
                       (funcall processor real-uri parent-url page-text)
                       (sleep crawl-delay))
                   (skip-page () nil)))))
      queue)))


#|
TODO:

Take the Content-location header into account when calculating the base URI.
I just don't know who actually uses it.

Handle the Crawl-delay tag in robots.txt.

Make crawl-delay take processing time into account.

Re-read robots.txt on specified intervals.

Crawl separate sites independently and concurrently (based on hostname?)

|#
