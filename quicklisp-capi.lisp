;;;; quicklisp-capi.lisp
;;
;;;; Copyright (c) 2019 Eric Lorenzana <eric.lorenzana@protonmail.ch>

(in-package #:quicklisp-capi)

(defun search-distributions (term)
  "From the given query string, return a list of distributions."
  (flet ((search-term (system)
           (search (string-downcase term)
                   (ql-dist:name system))))
    (loop for system in (ql-dist:provided-systems t)
          when (or (search-term system)
                   (search-term (ql::release system)))
          collect system)))

(defun make-quickdocks-url (distribution)
  "Return an URL in the form of string. This will be consumed later on
by capi:browser-pane."
  (format nil "http://quickdocs.org/~a/" distribution))

(capi:define-interface main-window ()
  ((distributions :accessor distributions
                  :initform '()
                  :documentation "The list of distributions for the current search."))
  (:panes
   (text-input-search
    capi:text-input-pane
    :title "Search distribution")
   (push-button-search
    capi:push-button
    :text "OK"
    :callback #'(lambda (data interface)
                  (declare (ignore data))
                  (setf (distributions interface)
                        (search-distributions
                         (capi:text-input-pane-text text-input-search)))
                  ;; First, remove all elements from the distribution
                  ;; list. Do so by sending a predicate that always
                  ;; returns a subclass of T.
                  (capi:remove-items list-panel-result #'(lambda (x) x))
                  ;; Perform a distribution search and append all
                  ;; elements to the list panel.
                  (capi:append-items list-panel-result
                                     (mapcar #'ql-dist:name (distributions interface)))))
   (list-panel-result
    capi:list-panel
    :action-callback #'(lambda (data interface)
                         (declare (ignore interface))
                         (capi:contain (make-instance 'capi:browser-pane
                                                      :url (make-quickdocks-url data))
                                       :title (format nil "~a | Quickdocks" data)
                                       :best-width 1024
                                       :best-height 768))
    :items '()))
  (:layouts
   (layout-main capi:column-layout
                '(layout-search
                  layout-result))
   (layout-search capi:row-layout
                  '(text-input-search
                    push-button-search)
                  :title "Search"
                  :title-position :frame)
   (layout-result capi:column-layout
                  '(list-panel-result)
                  :title "Result"
                  :title-position :frame))
  (:default-initargs :title "Quicklisp"
   :best-width 480
   :best-height 640))

(defun main ()
  (capi:display (make-instance 'main-window)))
