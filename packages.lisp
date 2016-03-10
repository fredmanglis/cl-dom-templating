(in-package :common-lisp-user)

(defpackage :ke.me.fredmanglis.cl-domt
  (:use :common-lisp
        :cxml
        :xpath)
  (:export :load-file
           :repeat
           :next
           :set
           :set-value))
