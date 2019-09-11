;;;; quicklisp-capi.lisp

(in-package #:quicklisp-capi)

(defun search-distributions (term)
  "From the given query string, return a list of distributions."
  (setf term (string-downcase term))
  (loop for system in (ql-dist:provided-systems t)
        when (or (search term (ql-dist:name system))
                 (search term (ql-dist:name (ql::release system))))
        collect system))