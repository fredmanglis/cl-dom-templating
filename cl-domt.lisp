(in-package :ke.me.fredmanglis.cl-domt)

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
    (setf (slot-value template 'dom) (cxml:parse-file path (cxml-dom:make-dom-builder)))))
