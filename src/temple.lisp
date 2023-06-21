(defpackage :temple
  (:use :common-lisp))

(in-package :temple)

; A symbol is a special variable if it can be evaluated.
(defun specialp (sym)
  "Checks if `sym' is a special variable (global)"
  (when (not (constantp sym))
   (handler-case (and (eval sym) t)
    (error () nil))))

(defun eval-at-runtimep (arg)
  (if (specialp arg)
      t
      (handler-case (and (eval arg) nil)
        (error () t))))

(defun interpolate-static-contents (contents)
  (let (str args)
    (loop for item in contents do
          (if (eval-at-runtimep item)
            (progn
              (setf str (concatenate 'string str "~a"))
              (uiop:appendf args (list item)))
            (setf str (concatenate 'string str (format nil "~a" item)))))
    (values str args)))

(defun interpolate-static-args (args)
  (when args
    (multiple-value-bind (str parameters) (interpolate-static-args (cddr args))
      (if (eval-at-runtimep (second args))
        (values (concatenate 'string str (format nil " ~a='~a'" (first args) "~a"))
                (push (second args) parameters))
        (values (concatenate 'string str (format nil " ~a='~a'" (first args) (second args)))
                nil)))))

(defmacro tag (key params-and-contents)
  "Generates an HTML `key' tag with parameters being keyword-value pairs and contents being the rest of values inside `params-and-contents'.
Interpolates as many expressions at compile time as possible. Special (global) variables won't be interpolated at compile time.

 E.g:  (tag \"a\" (:href \"hi\" \"hello\")) => \"<a href='hi'>hello</a>\" "
  (let ((prev) (args) (contents))
    (loop for val in params-and-contents do
      (cond
        ((keywordp prev)
         (when (find-if (lambda (x) (equal prev x)) args)
           (warn "Repeated arguments."))
         (uiop:appendf args (list (sb-unicode:lowercase (string prev)) val)))
        ((not (keywordp val)) (uiop:appendf contents (list val))))
      (setf prev val))

    ; evaluate any functions embedded in the html tag.
    (setf args (mapcar (lambda (x) (if (specialp x)
                                  x
                                  (handler-case (eval x) (error () x)))) args))
    (setf contents (mapcar (lambda (x) (if (specialp x)
                                      x
                                      (handler-case (eval x) (error () x)))) contents))

    ; Pregenerate as much text as possible at compile time.
    (multiple-value-bind (arg-template arg-params) (interpolate-static-args args)
      (multiple-value-bind (content-template content-params) (interpolate-static-contents contents)
          (let ((template (format nil "<~a~@[~a~]>~@[~a~]</~a>" key arg-template content-template key)))
            (cond
              ((and content-params arg-params) `(format nil ,template ,@arg-params ,@content-params))
              (arg-params `(format nil ,template ,@arg-params))
              (content-params `(format nil ,template ,@content-params))
              (t template)))))))

(defmacro deftags (&rest tags)
  (when tags
    `(progn
       (defmacro ,(car tags) (&rest params-and-contents)
         ,(format nil
                  "Generates an HTML `~(~a~)' tag with parameters specified as keyword-value pairs in params-and-contents and contents being the rest of the elements in `params-and-contents'.
                   E.g: (~(~a~) :class \"foo\") => <~(~a~) class='foo'><~(~a~)>" (car tags) (car tags) (car tags) (car tags))
         `(tag ,,(sb-unicode:lowercase (string (car tags))) ,params-and-contents))
       (export ',(car tags))
       (deftags ,@(cdr tags)))))

(deftags html head body span p div a ul ol h1 h2 h3)
