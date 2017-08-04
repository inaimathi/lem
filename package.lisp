;;;; package.lisp

(defpackage #:lem
  (:use #:cl)
  (:export
   :define-machine
   :get-state :set-state! :empty! :move-to! :spawn-in!

   :make-grid :seed! :show! :sshow! :play!

   :ray :line))
