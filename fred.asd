(asdf:defsystem #:fred
  :depends-on
  (#:s-xml
   #:drakma)
  :name "fred"
  :author "plkrueger"
  :maintainer "plkrueger"
  :licence "MIT"
  :description "Lisp Interface to Federal Reserve Economic Data (FRED®)"
  :components ((:file "currency")
               (:file "fred"
                :depends-on ("fred-package" "hist-date" "fred-classes"))
               (:file "fred-package"
                :depends-on ("hist-date" "currency"))
               (:file "fred-classes"
                :depends-on ("fred-package"))
               (:file "hist-date")))
