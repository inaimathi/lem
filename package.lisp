;;;; package.lisp

(defpackage #:lem
  (:use #:cl)
  (:export
   :define-machine
   :get-state :set-state! :empty! :empty? :occupant :unit-type :code :move-to! :spawn-in! :self :here :neighbor

   :make-grid :grid-width :grid-height :get-cell :step-grid! :seed! :show! :sshow! :play!

   :unit :ray :line :box
   :n*von-neumann :n*moore :n*extended))
