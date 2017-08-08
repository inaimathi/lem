;;;; package.lisp

(defpackage #:lem
  (:use #:cl)
  (:export
   :define-machine
   :get-state :set-state! :empty! :empty? :occupant :unit-type :move-to! :spawn-in!

   :make-grid :grid-width :grid-height :get-cell :step-grid! :seed! :show! :sshow! :play!

   :ray :line :box))
