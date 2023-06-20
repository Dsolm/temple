(defpackage :temple
  (:use :common-lisp))

(in-package :temple)

(defmacro tag (key params-and-contents)
  "Generates an HTML `key' tag with parameters being keyword-value pairs and contents being the rest of values inside `params-and-contents'.
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
    (setf args (mapcar (lambda (x) (if (listp x) (eval x) x)) args))
    (setf contents (mapcar (lambda (x) (if (listp x) (eval x) x)) contents))

    ; Pregenerate as much text as possible at compile time.
    (if (find-if #'symbolp contents)
        `(format nil ,(format nil "<~a ~{~a='~a'~}>~a</~a>" key args "~{~a~}" key) (list ,@contents))
        (format nil "<~a ~{~a='~a'~}>~@[~{~a~}~]</~a>" key args contents key))))

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
