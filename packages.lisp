(in-package :common-lisp-user)

(defpackage #:cl-dom-templating
  (:use :common-lisp
        :cxml
        :xpath)
  (:nicknames #:domt)
  (:export :repeat
           :next
           :set
           :set-value
           :html))
