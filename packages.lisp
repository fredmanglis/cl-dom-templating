(in-package :common-lisp-user)

(defpackage #:cl-dom-templating
  (:use :common-lisp
        :cxml
        :xpath)
  (:nicknames #:domt)
  (:export :load-file
           :repeat
           :next
           :set
           :set-value))
