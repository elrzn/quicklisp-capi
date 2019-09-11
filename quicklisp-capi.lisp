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

(defun callback-perform-search (data interface)
  "Perform a search with the text-input-search content."
  (declare (ignore data))
  (with-slots (text-input-search list-panel-result)
      interface
    (setf (distributions interface)
          (search-distributions (capi:text-input-pane-text text-input-search)))
    ;; First, remove all elements from the distribution list. Do so by
    ;; sending a predicate that always returns a subclass of T.
    (capi:remove-items list-panel-result #'(lambda (x) x))
    ;; Perform a distribution search and append all elements to the
    ;; list panel.
    (capi:append-items list-panel-result
                       (mapcar #'ql-dist:name (distributions interface)))))

(defun callback-install-distribution (data interface)
  "Install the selected distribution."
  (declare (ignore data))
  (with-slots (list-panel-result)
      interface
    (let ((selected-distribution (capi:choice-selected-item list-panel-result)))
      (ql:quickload selected-distribution)
      ;; TODO Actually display the output somewhere and check whether
      ;; the installation was successful. Hint: use capi:collect-pane
      ;; for displaying the installation output.
      (capi:display-message "Installed ~s" selected-distribution))))

(defun callback-display-quickdocks (data interface)
  "Open a browser pane and display the quickdocks page for the
selected distribution."
  (declare (ignore interface))
  (capi:contain (make-instance 'capi:browser-pane
                               :url (make-quickdocks-url data))
                :title (format nil "~a | Quickdocks" data)
                :best-width 1024
                :best-height 768))

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
    :callback #'callback-perform-search)
   (push-button-install
    capi:push-button
    :text "Install"
    :default-p t
    :callback #'callback-install-distribution)
   (list-panel-result
    capi:list-panel
    :action-callback #'callback-display-quickdocks
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
                  '(list-panel-result
                    push-button-install)
                  :title "Result"
                  :title-position :frame))
  (:default-initargs :title "Quicklisp"
   :best-width 480
   :best-height 640))

(defun main ()
  (capi:display (make-instance 'main-window)))
