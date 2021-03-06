;;;; quicklisp-capi.lisp

(in-package #:quicklisp-capi)

(defun search-distributions (term)
  "From the given query string, return a list of distributions.

An empty string will return a list of all distributions."
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

(defun callback-display-quickdocks (data interface)
  "Open a browser pane and display the quickdocks page for the
selected distribution."
  (declare (ignore interface))
  (capi:contain (make-instance 'capi:browser-pane
                               :url (make-quickdocks-url data))
                :title (format nil "~a | Quickdocks" data)
                :best-width 820
                :best-height 640))

(defun callback-filtering-search (interface)
  "React on filtering input by limiting the displayed distributions to
those matching the content of the field."
  (let* ((things (distributions interface)) ; maybe don't be so specific here
         (filtered-things
          (multiple-value-bind (regexp excludep)
              (capi:filtering-layout-match-object-and-exclude-p
               (main-window-distribution-filtering interface)
               nil)
            (if regexp
                (loop for thing in things
                      when (if (lispworks:find-regexp-in-string
                                regexp
                                (string thing))
                               (not excludep)
                             excludep)
                      collect thing)
              things))))
    (setf (capi:collection-items
           (main-window-list-panel-result interface))
          filtered-things)))

(capi:define-interface main-window ()
  ((distributions
    :accessor distributions
    :initform (mapcar #'ql-dist:name (search-distributions ""))
    :documentation "The list of distributions for the current search."))
  (:panes
   (distribution-filtering
    capi:filtering-layout
    :change-callback 'callback-filtering-search
    :reader main-window-distribution-filtering)
   (push-button-install
    capi:push-button
    :text "Install"
    :callback #'callback-install-distribution)
   (list-panel-result
    capi:list-panel
    :reader main-window-list-panel-result
    :action-callback #'callback-display-quickdocks
    :items distributions))
  (:layouts
   (layout-main capi:column-layout
                '(layout-search
                  layout-result))
   (layout-search capi:row-layout
                  '(distribution-filtering)
                  :title "Search"
                  :title-position :frame)
   (layout-result capi:column-layout
                  '(list-panel-result
                    push-button-install)
                  :title "Distributions"
                  :title-position :frame))
  (:default-initargs :title "Quicklisp"
   :best-width 480
   :best-height 640))

(defun main ()
  (capi:display (make-instance 'main-window)))
