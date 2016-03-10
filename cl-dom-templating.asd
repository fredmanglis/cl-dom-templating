(defsystem "cl-dom-templating"
  :description "A package that makes use of xml methods to animate a html template"
  :author      "Muriithi Frederick Muriuki fredmanglis@gmail.com"
  :version     "0.0.0.1"
  :licence     "GPLv3"
  :depends-on  ("cxml" "xpath")
  :components  ((:file "packages")
                (:file "cl-dom-templating" :depends-on ("packages"))))
