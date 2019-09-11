;;;; quicklisp-capi.lisp

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

(capi:define-interface main-window ()
  ()
  (:panes
   (text-input-search capi:text-input-pane
                 :title "Search distribution")
   (push-button-search capi:push-button
                       :text "OK"
                       :callback #'(lambda (data interface)
                                     (declare (ignore data interface))
                                     nil ; nyi
                                     ))
   (list-panel-result capi:list-panel
                      :items '(work in progress)))
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
