(defpackage :future-asd
  (:use :cl :asdf))

(in-package :future-asd)

(defsystem :future
  :name "future"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Futures/Promises for ClozureCL."
  :serial t
  :components ((:file "future")))
