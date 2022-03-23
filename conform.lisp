;;;; Copyright 2020 Mark Polyakov
;;;; Released under the MIT license

(in-package #:conform)

;;;;;;;;;;;;;;;;;;;;;;;
;;  PLACE UTILITIES  ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Example: (defun my-incf (sth)
;;            (with-places sth
;;              (setf sth (1+ sth))))
;;
;;          (my-incf (place (cdadr *foo*)))

(defmacro place (place &environment env)
  "Given a place, expands into a form that evaluates to a cons whose car is a function that reads
from place and whose cdr is a function that writes to place."
  (multiple-value-bind (vars vals stores writer reader)
      (get-setf-expansion place env)
    `(let ,(mapcar #'list vars vals)
       (cons (lambda () ,reader)
             (lambda (new)
               (let ((,(car stores) new))
                 ,writer))))))

(defmacro with-places (var-or-vars &body body)
  "Given any number of variables that are currently bound to outputs of the (place) macro, evaluate
body with those variables bound to the value at that place, and, after executing body, save the
value of those variables back to the original place."
  (let ((vars-gensyms (loop for var in (ensure-list var-or-vars)
                         do (assert (symbolp var) (var) "Vars passed to with-places not symbols!")
                         collect (cons var (gensym (symbol-name var))))))
    `(let ,(loop for (var . gs) in vars-gensyms
              collect `(,gs ,var))
       (symbol-macrolet ,(loop for (var . gs) in vars-gensyms
                            collect `(,var (place-dereference ,gs)))
         ,@body))))

(defun place-dereference (sth)
  (funcall (car sth)))

(defsetf place-dereference (sth) (store)
  `(funcall (cdr ,sth) ,store))

;;;;;;;;;;;;;;;;;;;;;;
;;  CORE FRAMEWORK  ;;
;;;;;;;;;;;;;;;;;;;;;;

;; *event-handlers* is a list of cons cells:
;;
;; + An event handler function, which takes a Value Iterator as an argument, gets all the values it
;; needs, then returns another function, which performs any side effects of the event handler. By
;; having two functions we can call the value iterators in one order, but effect the side effects in
;; a different order. The value iterators must be called in the same order as the name iterators
;; during the render phase, hence this requirement. The event handler will not be called until the
;; render function has been called, so the render function may register event handlers.
;;
;; + A number, a relative order in which event handler side effects will be effected.
(defvar *event-handlers*)
(defvar *name-iterator*)
(defvar *value-iterator*)
(defvar *form-errors*)

(defun assert-conformlet-context (fun-name)
  (assert (boundp '*event-handlers*) nil "~A called outside of render-conformlet!" fun-name)
  (assert (boundp '*name-iterator*) nil "~A called during the event-handling phase!" fun-name))

(defmacro conformlet ((&key (order 0) places names) &body body
                      &aux (names (ensure-list names))
                        (new-event-var (gensym "NEW-EVENT-VAR")))
  "Expand into a form that evaluates body in the dynamic context of a new conformlet. The -let
naming scheme has nothing to do with other -let functions; it is merely a modification of the name
``formlet''. And unlike -let functions, conformlet establishes a dynamic, not lexical, context.

Conformlet must be called from the dynamic context of another conformlet or of render-conformlet.

``Order'' indicates is a number indicating the relative order that the event handlers defined in and
under this conformlet will be evaluated compared to sibling conformlets (direct children of the same
parent conformlet parent as the one being defined).

The ``Places'', if provided, are bound using ``with-places''. This is a convenience, since places
are frequently used with conformlets. Providing places is equivalent to manually wrapping the
conformlet call inside with-places.

All ``Names'', if provided, are, during rendering, bound to strings that are suitable for use as the
``name'' attributes on HTML input elements, and, during event handling, are bound to the
corresponding POST values, as returned by the post-getter argument to render-conformlet."
  (once-only (order)
    `(with-places ,places
       (assert-conformlet-context "(conformlet)")
       (let (,new-event-var)
         (multiple-value-prog1
             (let (*event-handlers*
                   ,@(loop for name in names
                        collect `(,name (funcall *name-iterator*))))
               (multiple-value-prog1
                   (locally ,@body)
                 ;; capture the dynamic value
                 (let ((event-handlers *event-handlers*))
                   (setf ,new-event-var
                         (cons
                          (lambda ()
                            ;; First, all the names we need
                            ,@(loop for name in names
                                 collect `(setf ,name (funcall *value-iterator*)))
                            ;; Second, the names our descendants need, in the order of instantiation.
                            (let ((instantiated-event-handlers
                                   (loop for event-handler in (nreverse event-handlers)
                                      collect (cons (funcall (car event-handler))
                                                    (cdr event-handler)))))
                              (lambda (&aux (sorted-instantiated-event-handlers
                                             (stable-sort
                                              instantiated-event-handlers
                                              (lambda (a b)
                                                (declare ((or number (eql :epilogue)) a b))
                                                (cond
                                                  ((eql a b) nil)
                                                  ((eql :epilogue b) t)
                                                  (t (< a b))))
                                              :key #'cdr)))
                                ;; Finally, call the event handler bodies in the order specified.
                                (loop for event-handler in sorted-instantiated-event-handlers
                                   do (funcall (car event-handler))))))
                          ,order)))))
           (push ,new-event-var *event-handlers*))))))

(defmacro custom-event (&body body)
  "Expand into a form that registers a custom event handler consisting of the forms in body. This
event handler will be called after all non-custom event handlers in the parent conformlet."
  `(progn
     (assert-conformlet-context "(custom-event)")
     (push (cons (lambda () (lambda () ,@body)) :epilogue)
           *event-handlers*)))

(defmacro render-form (prefix value-getter submission-p &body body)
  "Sets up a dynamic environment in which (conformlet) calls are allowed, and return the HTML of the
rendered form. ``prefix'' is prepended to the names of input elements to allow multiple forms to
coexist on the same page. ``value-getter'' is a function that, given an <input> name, returns the
corresponding value. ``submission-p'' should be non-nil when the form has been submitted. When nil,
events will not be processed."
  (once-only (prefix value-getter submission-p)
    `(let (*event-handlers*
           *form-errors*)
       (when ,submission-p
         ;; First render stage
         (let ((*name-iterator* (make-name-iterator ,prefix)))
           (conformlet () ,@body))

         ;; Event handling stage
         (let ((*value-iterator* (compose ,value-getter (make-name-iterator ,prefix))))
           ;; conformlet pushes exactly one event handler. Let's call it!
           (funcall (funcall (caar *event-handlers*)))))

       ;; Second render stage. Event handlers have already been called, so we don't even bother
       ;; rebinding *event-handlers*!
       (let ((*name-iterator* (make-name-iterator ,prefix)))
         (conformlet () ,@body)))))

(defun make-name-iterator (prefix)
  (let ((i 0))
    (lambda ()
      (format nil "~a~a" prefix (incf i)))))

;;;;;;;;;;;;;;
;;  FIELDS  ;;
;;;;;;;;;;;;;;

(defvar *field-classes* nil
  "List of CSS classes to put on all fields (the outer <div> that surrounds both the <label> and the
input")

(defvar *string-classes* nil
  "List of CSS classes to put on the <input> or <textarea> of string-field instances")

(defvar *select-classes* nil
  "List of CSS classes to put on the <select> of select-field instances")

(defvar *radio-container-classes* nil
  "List of CSS classes to put on the <div> surrounding a group of radio labels & inputs")

(defvar *radio-label-classes* nil
  "List of CSS classes to put on the <label> that contains each radio field")

(defun class-attributes (classes)
  `(class ,(format nil "~{~a~^ ~}" classes)))

;;; -input conformlets return a cons: First, the inner HTML, and second, the name attribute used, so
;;; that a parent can use it in, say, a <label>. Multiple values wouldn't work because (conform) is a lexical /macro/, not function, so even though it expands into a function call, it

(defun string-input (val &optional textarea html-attrs)
  (conformlet (:places val :names name)

    (custom-event
      ;; do not update val when val-iter returns nil
      (when name
        (setf val name)))

    (values
     `(,(if textarea 'textarea 'input)
        (name ,name id ,name ,@(when (and (not textarea) (stringp val))
                                 `(value ,val)) ,@html-attrs)
        ,(when (and textarea (stringp val))
           val))
     name)))

(defun select-like-input (val options multiple outer-fn inner-fn)
  (conformlet (:places val :names name)

    (custom-event
      ;; TODO: error handling for parse integer?
      (when name
        (setf val
              (flet ((str->option (str)
                       (car (nth (parse-integer str) options))))
                (if multiple
                    (mapcar #'str->option name)
                    (str->option name))))))

    (values
     (funcall outer-fn name
              (loop for (option-val option-text) in options
                 for i from 0
                 for selected = (member option-val (if multiple val (list val)))
                 collect (funcall inner-fn name i selected option-text)))
     name)))

(defun select-input (val options &optional multiple select-attrs option-attrs)
  (select-like-input val options multiple
                     (lambda (name inner-html)
                       `(select (name ,name ,@(when multiple '(multiple t)) ,@select-attrs)
                                ,inner-html))
                     (lambda (name value selected text)
                       (declare (ignore name))
                       `(option (value ,value ,@(when selected '(selected t)) ,@option-attrs)
                                ,text))))

(defun radio-input (val options &optional div-attrs label-attrs input-attrs)
  (select-like-input val options nil
                     (lambda (name inner-html)
                       (declare (ignore name))
                       `(div ,div-attrs
                             ,inner-html))
                     (lambda (name value selected text)
                       `(label (for ,name ,@label-attrs)
                               (input (type "radio" name ,name value ,value
                                            ,@(when selected '(checked t))
                                            ,@input-attrs))
                               ,text))))

(defun checkbox-input (val &optional html-attrs)
  (conformlet (:places val :names name)
    (custom-event
      ;; unlike string-conformlet, we unconditionally 
      (setf val (not (not name))))

    (values
     `(input (type "checkbox" value "T" name ,name id ,name
                   ,@(when val
                       '(checked t))
                   ,@html-attrs))
     name)))

(defun button (onclick text &rest html-attrs)
  "Simple button conformlet. All buttons are submit buttons."
  (conformlet (:order 1 :names name)

    (custom-event
      (when name
        (funcall onclick)))

    `(button (name ,name id ,name type "submit" ,@html-attrs)
             ,text)))

;;; -field conformlets are higher-level than -input ones.

;; TODO: different "styles" so that we can optionally get rid of the wrapper div, for example.
(defun make-field (label name inner-html)
  `(div (,@(class-attributes *field-classes*))
        (label (for ,name) ,label)
        ,inner-html))

(defun field (val label validate error input-conformlet)
  "Wraps an -input value conformlet with the given label."
  (declare (function input-conformlet
                     validate)
           (string error))
  (conformlet (:places val)
    (multiple-value-bind (input-html name)
        (funcall input-conformlet
                 (cons (lambda () val)
                       (lambda (new-val)
                         (if (funcall validate new-val)
                             (setf val new-val)
                             (push error *form-errors*)))))
      (make-field label name input-html))))

(defun string-field (val label
                     &rest html-attrs
                     &key textarea
                         (validate (constantly t)) (error "String validation error")
                         &allow-other-keys)
    "A string field, with the given label. validate checks the field's validity and error is the
message to push on failure. textarea, if set, causes a <textarea> to be used instead of <input>."
  (delete-from-plistf html-attrs :validate :error :textarea)
  (field val label validate error (rcurry #'string-input textarea html-attrs)))

(defun select-field (val label options
                     &key (validate (constantly t)) (error "Select validation error") multiple)
  ;; TODO: docs
  (field val label validate error
         (rcurry #'select-input options multiple (class-attributes *select-classes*) nil)))

(defun radio-field (val label options
                    &key (validate (constantly t)) (error "Select validation error"))
  ;; TODO: docs
  (field val label validate error
         (rcurry #'radio-input options
                 (class-attributes *radio-container-classes*)
                 (class-attributes *radio-label-classes*)
                 nil)))

(defun confirm-password-field (val label-1 label-2
                               &key
                                 (validate (constantly t))
                                 (error "Password validation failed")
                                 (confirm-error "Passwords did not match")
                                 suppress-error-when-empty)
  (conformlet (:places val)
    (let ((pw1 "")
          (pw2 ""))

      (custom-event
        (let ((ok t))
          (unless (or (and (emptyp pw1) suppress-error-when-empty)
                      (funcall validate pw1))
            (push error *form-errors*)
            (setf ok nil))
          (unless (equal pw1 pw2)
            (push confirm-error *form-errors*)
            (setf ok nil))
          (when ok
            (setf val pw1))))

      `(,(string-field (place pw1) label-1 :type "password")
         ,(string-field (place pw2) label-2 :type "password")))))

(defun checkbox-field (val label &key (validate (constantly t)) (error "Checkbox validation error"))
  (field val label validate error
         #'checkbox-input))

(defgeneric default-conformlet (val val-place)
  (:documentation "Instantiate a conformlet for the given value. The value is passed as the first
argument, then the place where that value is stored as the next argument, then other arguments
later."))

(defun auto-conformlet (val)
  "Given a place (using the (place) macro), call the appropriate conformlet."
  (default-conformlet (funcall (car val)) val))

