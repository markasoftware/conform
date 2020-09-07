;;;; conform.lisp

(in-package #:conform)

;;;;;;;;;;;;;;;;;
;;  FRAMEWORK  ;;
;;;;;;;;;;;;;;;;;

;; iterator: list (parts prefix &optional getter)

(defun make-name-iterator (prefix)
  (list (list 0) prefix))

(defun make-value-iterator (prefix getter)
  (list (list 0) prefix getter))

;; changes the iterator in-place
(defun iterator-next (iterator)
  (incf (caar iterator))
  iterator)

;; returns a new iterator, leaving the original untouched, and also not being changed when
;; iterator-next is called on the original.
(defun iterator-into (iterator)
  (cons (cons 0 (copy-list (first iterator))) (cdr iterator)))

(defun iterator-name (iterator)
  (format nil "~a~{~a~^-~}" (second iterator) (first iterator)))

(defun iterator-value (iterator)
  (assert (third iterator) nil "Can't get value of a name iterator")
  (funcall (third iterator) (iterator-name iterator)))

(defmacro conformlet ((&key
                       ((:val val-var))
                       (order 0)
                       post-names
                       extra-args)
                      &body body)

  ;; children: ((event-fn default-order explicit-order) (event-fn-2 do2 eo2))
  (declare (symbol val-var)
           (list extra-args post-names))

  (with-gensyms (name-iter-var onsubmit-var val-provided-var)
    (once-only (order)
      (with-gensyms (children-var)
        `(lambda (&key
                    ,@(when val-var
                        `(((:val ,val-var) nil ,val-provided-var)
                          ((:onsubmit ,onsubmit-var))))  
                    ,@extra-args
                  &aux ,children-var)

           ;; check data instance parameters provided when this is a data conformlet
           ,@(when val-var
               `((declare (function ,onsubmit-var))
                 (assert ,val-provided-var nil ":val must be provided to value conformlets.")))

           ;; TODO: don't put post-names out in the cold like this! The problem is the post-names
           ;; must be accessible from the custom-events, which are defined lexically inside the
           ;; render function, but the values are not known until the event handling function, so we
           ;; define them lexically at the top level so they can be accessed and set from both
           ;; render and event handling. I'm worried that somehow the render function might end up
           ;; with a reference to a variable after it is changed by the event handling stage. It
           ;; might be better to pass the post-name variables as arguments to the custom event
           ;; functions?
           (let ,post-names
             (list
              (lambda (,name-iter-var)
                ,@(loop for post-name in post-names
                     collect `(setf ,post-name (iterator-name (iterator-next ,name-iter-var))))
                (flet
                    ((conform-raw (order conformlet-instance)
                       (declare (cons conformlet-instance)
                                ((or number (member :epilogue)) order))
                       ;; register the event handler.
                       (push (append (cdr conformlet-instance) (list order))
                             ,children-var)
                       ;; ...and actually render the body
                       (funcall (car conformlet-instance) (iterator-into (iterator-next ,name-iter-var)))))

                  (macrolet
                      ((conform (conformlet
                                    &rest args
                                    &key val onsubmit (order 0)
                                    &allow-other-keys
                                    &aux (extra-args (remove-from-plist args :val :onsubmit :order))
                                    &environment env)
                         ;; we do /not/ once-only on extra-args. once-only'ing the whole list (with
                         ;; the /forms/ for each value) is not useful -- we'd need to once-only each
                         ;; value, which is more trouble than just being careful not to eval it
                         ;; twice.
                         (once-only (order conformlet)
                           (if val
                               (multiple-value-bind (vars vals stores writer reader)
                                   (get-setf-expansion val env)
                                 ;; TODO: declarations to check type of conformlet
                                 `(let ,(mapcar #'list vars vals) 
                                    (conform-raw
                                     ,order
                                     (funcall
                                      ,conformlet
                                      :val ,reader
                                      :onsubmit ,(or onsubmit
                                                     ;; TODO: double check that v won't conflict
                                                     ;; with variables in the writer.
                                                     `(lambda (v)
                                                        (let ((,(car stores) v))
                                                          ,writer)))
                                      ,@extra-args))))
                               `(conform-raw ,order (funcall ,conformlet ,@extra-args)))))

                       (custom-event ((&key (order :epilogue))
                                      &body body)
                         (once-only (order)
                           (with-gensyms (val-iter-var)
                             `(conform-raw
                               ,order
                               (list (constantly nil) ; no render function
                                     (lambda (,val-iter-var)
                                       (declare (ignore ,val-iter-var))
                                       ,@body) ; event fn
                                     0))))))   ; default order

                    ,@(if val-var
                          `((multiple-value-prog1
                                (locally ,@body)
                              ;; evaluating this last means it is the very last event to fire
                              (custom-event ()
                                            (funcall ,onsubmit-var ,val-var))))
                          body))))

              (lambda (val-iter
                       &aux (children (nreverse ,children-var)))
                ,@(loop for post-name in post-names
                     collect `(setf ,post-name (iterator-value (iterator-next val-iter))))

                ;; value iterators must be bound to each event handler in the order
                ;; the event handlers were registered (not necessarily the order
                ;; they will be executed), so that they are called in the same order
                ;; the name iterators were called.
                (dolist (child children)
                  (rplaca child (curry (first child) (iterator-into (iterator-next val-iter)))))

                ;; most recent child is first before the sort, we want it to end up last
                (dolist (child (stable-sort
                                children
                                (lambda (child-a child-b)
                                  ;; child format: (event default-order explicit-order)
                                  (let ((explicit-eql (eql (third child-a)
                                                           (third child-b)))
                                        (explicit-numerical (and (numberp (third child-a))
                                                                 (numberp (third child-b)))))
                                    (or
                                     ;; put epilogues last
                                     (and (not explicit-eql) (eq :epilogue (third child-b)))
                                     ;; then sort by explicit order
                                     (and explicit-numerical (< (third child-a)
                                                                (third child-b)))
                                     ;; then sort by default order
                                     (and explicit-eql (< (second child-a)
                                                          (second child-b))))))))
                  (funcall (first child))))
              ,order)))))))

(defvar *form-errors* nil
  "A list of validation errors (strings) collected while processing the form submission.")

;; TODO: custom name for the form
(defmacro render-form (prefix post-getter conformlet &rest instance-args)
  "post-getter is a function that, given a string, gets the corresponding POST value (eg,
#'hunchentoot:post-parameter). name-prefix, if provided, is used to distinguish fields of this form
from fields of other forms in the same HTML page."
  (once-only (prefix post-getter conformlet)
    `(let ((*form-errors* nil))
       ;; render and handle-events are bound after evaluating the instance args, so there's no
       ;; chance of clobbering variables.
       (destructuring-bind (render handle-events order)
           (funcall (conformlet () (conform ,conformlet ,@instance-args)))
         (declare (ignore order))
         (funcall render (make-name-iterator ,prefix))
         (funcall handle-events (make-value-iterator ,prefix ,post-getter))
         (funcall render (make-name-iterator ,prefix))))))

;;;;;;;;;;;;;;
;;  FIELDS  ;;
;;;;;;;;;;;;;;

(defvar *field-classes* nil
  "List of HTML/CSS classes to put on all fields (the outer <div> that surrounds both the <label>
  and the input")

(defvar *string-classes* nil
  "List of HTML/CSS classes to put on the <input> or <textarea> of string-field instances")

(defvar *select-classes* nil
  "List of HTML/CSS classes to put on the <select> of select-field instances")

(defun class-attributes (classes)
  `(class ,(format nil "~{~a~^ ~}" classes)))

;;; -input conformlets return a cons: First, the inner HTML, and second, the name attribute used, so
;;; that a parent can use it in, say, a <label>. Multiple values wouldn't work because (conform) is a lexical /macro/, not function, so even though it expands into a function call, it

(defun string-input (&optional html-attrs (html-tag 'input))
  (conformlet (:val val :post-names (name))

    (custom-event ()
                  ;; do not update val when val-iter returns nil
                  (when name
                    (setf val name)))

    (values
     `(,html-tag (name ,name ,@(when (stringp val)
                                 `(value ,val)) ,@html-attrs))
     name)))

(defun select-input (options &optional select-attrs option-attrs multiple)
  (conformlet (:val val :post-names (name))

    (custom-event ()
                  ;; TODO: error handling for parse integer?
                  (when name
                    (setf val
                          (flet ((str->option (str)
                                   (car (nth (parse-integer str) options))))
                            (if multiple
                                (mapcar #'str->option name)
                                (str->option name))))))

    (values
     `(select (name ,name ,@(when multiple `(multiple t)) ,@select-attrs)
              ,(loop for (option-val option-text) in options
                  for i from 0
                  for selected = (member option-val (if multiple val (list val)))
                  collect `(option (value ,i
                                          ,@(when selected
                                              `(selected t))
                                          ,@option-attrs)
                                   ,option-text)))
     name)))

(defun button (text &rest html-attrs)
  (conformlet (:extra-args (onclick) :order 1 :post-names (name))

    (custom-event ()
                  (when name
                    (funcall onclick)))

    `(button (name ,name type "submit" ,@html-attrs)
             ,text)))

;;; -field conformlets are higher-level than -input ones.

;; TODO: different "styles" so that we can optionally get rid of the wrapper div, for example.
(defun make-field (label name inner-html)
  `(div (,@(class-attributes *field-classes*))
        (label (for ,name) ,label)
        ,inner-html))

(defun field (label validate error input-conformlet)
  "Wraps an -input value conformlet with the given label."
  (declare (string label)
           (function input-conformlet
                     validate)
           (string error))
  (conformlet (:val val)
    (multiple-value-bind (input-html name)
        (conform input-conformlet
                 :val val
                 :onsubmit (lambda (new-val)
                             (if (funcall validate new-val)
                                 (setf val new-val)
                                 (push error *form-errors*))))
      (make-field label name input-html))))

(defun string-field (label
                     &rest html-attrs
                     &key textarea
                       (validate (constantly t)) (error "String validation error")
                       &allow-other-keys)
  "A string field, with the given label. validate checks the field's validity and error is the
message to push on failure. textarea, if set, causes a <textarea> to be used instead of <input>."
  (delete-from-plistf html-attrs :validate :error :textarea)
  (field label validate error (string-input html-attrs (if textarea 'textarea 'input))))

(defun select-field (label options
                     &key (validate (constantly t)) (error "Select validation error") multiple)
  ;; TODO: docs
  (field validate error label
         (select-input options (class-attributes *select-classes*) nil multiple)))

(defun confirm-password-field (label-1 label-2
                               &key
                                 (validate (constantly t))
                                 (error "Password validation failed")
                                 (confirm-error "Passwords did not match")
                                 suppress-error-when-empty)
  (conformlet (:val val)
    (let ((pw1 "")
          (pw2 ""))

      (custom-event ()
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

      `(,(conform (string-field label-1 :type "password")
                  :val pw1)
         ,(conform (string-field label-2
                                 :type "password")
                   :val pw2)))))

(defgeneric default-conformlet (val)
  (:documentation "Return a default conformlet for the value. See auto-conformlet"))

(defun auto-conformlet ()
  (conformlet (:val val)
    (conform (default-conformlet val)
             :val val)))

