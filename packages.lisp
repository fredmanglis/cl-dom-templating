(in-package :common-lisp-user)

(defpackage #:cl-dom-templating
  (:use :common-lisp
        :cxml
        :xpath)
  (:nicknames #:domt)
  (:export :html
           :set-value
           :query
           :remove
           :repeat
           :next))
