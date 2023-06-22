(defpackage :temple
  (:use :common-lisp))

(in-package :temple)

(defun callablep (list)
  (and (listp list) (symbolp (car list))
       (not (equal (car list) 'quote))
       (not (equal (car list) 'list))))

; A symbol is a special variable if it can be evaluated.
(defun specialp (var)
  "Checks if `var' is a special variable (global) or contains one."
  (cond ((and (symbolp var) (not (constantp var))) (handler-case (and (eval var) t) (error () nil)))
        ((listp var) (when (find-if #'specialp var) t))))

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
              (setf str (concatenate 'string str "~@[~{~a~}~]"))
              (uiop:appendf args (list item)))
            (setf str (concatenate 'string str (format nil "~a" item)))))
    (values str args)))

; TODO: interpolate-static-args does not work with runtime lists.
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

  ; evaluate any functions embedded in the html tag.
  (setf params-and-contents (mapcar (lambda (x)
                                      (if (equal :comptime (and (listp x) (car x)))
                                        (eval (cdr x))
                                        (if (specialp x)
                                            x
                                            (handler-case (eval x) (error () x))))) params-and-contents))

  (let ((prev) (args) (contents))
    (loop for val in params-and-contents do
      (when val
        (cond
          ((keywordp prev)
           (when (find-if (lambda (x) (equal prev x)) args)
             (warn "Repeated arguments."))
           (uiop:appendf args (list (sb-unicode:lowercase (string prev)) val)))
          ((and (not (keywordp val)) (listp val) (not (callablep val))) (uiop:appendf contents val))
          ((not (keywordp val)) (uiop:appendf contents (list val)))))
      (setf prev val))

    ; Pregenerate as much text as possible at compile time.
    (multiple-value-bind (arg-template arg-params) (interpolate-static-args args)
      (multiple-value-bind (content-template content-params) (interpolate-static-contents contents)
        (let ((template (format nil "<~a~@[~a~]>~@[~a~]</~a>" key arg-template content-template key)))
          (cond
            ((and content-params arg-params) `(format nil ,template ,@arg-params ,@(loop for param in content-params
                                                                                         collect `(list ,param))))
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
