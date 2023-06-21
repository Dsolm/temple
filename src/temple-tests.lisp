(defpackage :temple/tests
  (:use :cl
        :temple
        :fiveam))

(in-package :temple/tests)

(def-suite temple)
(in-suite temple)

(test tags
  (is (equal (a :href (+ 1 2) (+ 1 2)) "<a href='3'>3</a>"))
  (is (equal (a :href "hi" "hi") "<a href='hi'>hi</a>"))
  (is (equal (a) "<a></a>"))
  (is (equal (a (+ 1 (+ 1 1))) "<a>3</a>"))
  (is (equal (a :href (+ 1 (+ 1 1)) :class (+ 1 (+ 1 1)) (+ 1 (+ 1 1)) (+ 1 3)) "<a class='3' href='3'>34</a>"))
  (is (equal (macroexpand '(a :class nunu :href "hi" "hi" *nunu*)) '(format nil "<a href='hi' class='~a'>hi~[~{~a~}~]</a>" nunu (list *nunu*)))))
