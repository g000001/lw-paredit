;;;; -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem lw-paredit
  :version "1"
  :description "Paredit version 1 for LispWorks"
  :long-description "Paredit version 1 for LispWorks"
  :author "Taylor Campbell"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on ()
  :components ((:file "pkgdcl")
               (:file "emacsify")
               (:file "paredit")))


(defmethod perform :after ((o load-op) (c (eql (find-system :lw-paredit))))
  (let ((name "https://github.com/g000001/lw-paredit")
        (nickname 'paredit))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


;;; *EOF*
