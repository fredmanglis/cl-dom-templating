(in-package :cl-dom-templating)

(defclass domtemplate ()
  ((file-path
    :accessor file-path
    :initarg :file-path
    :initform (error "Must supply the file path")
    :documentation "Provide the path to the file to be parsed into the DOM")
   (dom
    :accessor dom
    :documentation "The variable holding the DOM representation of the file provided by file-path")))

(defmethod initialize-instance :after ((template domtemplate) &key)
  (let ((path (file-path template)))
    (setf (slot-value template 'dom) (cxml:parse (pathname path) (cxml-dom:make-dom-builder)))))

(defgeneric html (template)
  (:documentation "Outputs the DOM as a HTML string"))

(defgeneric query (template xpath)
  (:documentation "Returns the elements that match the passed query"))

(defgeneric set-value (template &key xpath value)
  (:documentation "Updates the value of the elements matching the given xpath expression"))

(defmethod html ((template domtemplate))
  (with-output-to-string (s)
    (format s "~a~%~a~%"
            "<!DOCTYPE html>"
            (dom:map-document
             (cxml:make-string-sink :omit-xml-declaration-p t)
             (dom template)))
    s))

(defmethod query ((template domtemplate) xpath)
  (get-dom-element-by-xpath template xpath))

(defmethod set-value ((template domtemplate) &key xpath value)
  (loop for element in (get-dom-element-by-xpath template xpath)
     do
       (setf (dom:node-value element) value)))

(defun get-dom-element-by-xpath (template xpath)
  (let ((nodeset (xpath:evaluate xpath (dom template))))
    (let ((iterator (xpath:make-node-set-iterator nodeset))
          (elements ()))
      (loop
         for element = (xpath:node-set-iterator-current iterator)
         then (advance-iterator-and-return-value iterator)
         while element
         do
           (setf elements (append elements (list element))))
      elements)))

(defun advance-iterator-and-return-value (iterator)
  (xpath:node-set-iterator-next iterator)
  (xpath:node-set-iterator-current iterator))

(defmethod (setf dom:node-value) :after (newval (self dom:element))
  (let ((child-nodes (dom:child-nodes self))
        (new-child (dom:create-text-node (dom:owner-document self) newval)))
    (loop for i from 0 upto (- (length child-nodes) 1)
       do
         (let ((old-child (elt child-nodes 0)))
           (dom:remove-child self old-child)))
    (dom:append-child self new-child)))
