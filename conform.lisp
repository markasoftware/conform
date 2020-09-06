;;;; conform.lisp

(in-package #:conform)

;;;;;;;;;;;;;;;;;
;;  FRAMEWORK  ;;
;;;;;;;;;;;;;;;;;

(defmacro conformlet ((&key
                       ((:val val-var))
                       ((:onsubmit onsubmit-var) (gensym "ONSUBMIT") explicit-onsubmit-p)
                       (order 0)
                       ((:name-iter name-iter-var) (gensym "NAME-ITER"))
                       extra-args)
                      &body body
                      &aux (val-provided-var (gensym "VAL-PROVIDED-VAR")))
  ;; children: ((event-fn default-order explicit-order) (event-fn-2 do2 eo2))
  (declare (symbol val-var onsubmit-var name-iter-var)
           (list extra-args))
  (assert (or val-var (not explicit-onsubmit-p)) nil
          "Cannot specify onsubmit without val! Make up some other evt handler name!")
  (once-only (order)
    (with-gensyms (children-var)
      ;; parenthesis hell?
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

         (list
          (lambda (,name-iter-var)
            (flet
                ((conform-raw (order conformlet-instance)
                   (declare (cons conformlet-instance)
                            ((or number (member :epilogue)) order))
                   ;; register the event handler... (cdr formlet) => (event-handler default-order)
                   (push (append (cdr conformlet-instance) (list order)) ,children-var)
                   ;; ...and actually render the body
                   (funcall (car conformlet-instance) ,name-iter-var))
                 (,name-iter-var ()
                   (funcall ,name-iter-var)))

              (macrolet
                  ((conform (conformlet
                                &rest args
                                &key val onsubmit (order 0)
                                &allow-other-keys
                                &environment env
                                ;; TODO: figure out conflict between alexandria and metatilities
                                &aux (extra-args (remove-from-plist args :val :onsubmit :order)))
                     ;; we do /not/ once-only on extra-args. once-only'ing the whole list (with the
                     ;; /forms/ for each value) is not useful -- we'd need to once-only each value,
                     ;; which is more trouble than just being careful not to eval it twice.
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
                                                 ;; TODO: double check that v won't conflict with
                                                 ;; variables in the writer.
                                                 `(lambda (v)
                                                    (let ((,(car stores) v))
                                                      ,writer)))
                                  ,@extra-args))))
                           `(conform-raw ,order (funcall ,conformlet ,@extra-args)))))

                   (custom-event ((&key (order :epilogue)
                                        ((:val-iter val-iter-var) (gensym "VAL-ITER")))
                                  &body body)
                     (once-only (order)
                       `(conform-raw ,order (list (constantly nil) ; no render function
                                                  (lambda (,val-iter-var)
                                                    (flet ((,val-iter-var ()
                                                             (funcall ,val-iter-var)))
                                                      ,@body)) ; event fn
                                                  0)))))       ; default order

                ,@(if (and val-var (not explicit-onsubmit-p))
                      `((prog1
                            (locally ,@body)
                          ;; evaluating this last means it is the very last event to fire
                          (custom-event ()
                                        (funcall ,onsubmit-var ,val-var))))
                      body))))
          (lambda (val-iter)
            ;; most recent child is first before the sort, we want it to end up last
            (dolist (child (stable-sort
                            (nreverse ,children-var)
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
              (funcall (first child) val-iter)))
          ,order)))))

(defun make-name-iterator (prefix)
  (let ((i 0))
    (lambda ()
      (format nil "~a~a" prefix (incf i)))))

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
         (funcall handle-events (compose ,post-getter (make-name-iterator ,prefix)))
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

(defun string-input (html-attrs &optional (html-tag 'input))
  (conformlet (:val val :onsubmit onsubmit :name-iter name-iter)
              
    (custom-event (:val-iter val-iter)
                  ;; do not call onsubmit when, for example, the form is being loaded with GET
                  (when-let ((new-val (val-iter)))
                    (funcall onsubmit new-val)))

    (let ((name (funcall name-iter)))
      (values
       `(,html-tag (name ,name ,@(when (stringp val)
                                   `(value ,val)) ,@html-attrs))
       name))))

(defun select-input (options select-attrs option-attrs &optional multiple)
  (conformlet (:val val :onsubmit onsubmit :name-iter name-iter)

    (custom-event (:val-iter val-iter)
                  ;; TODO: error handling for parse integer?
                  (when-let ((new-val (val-iter)))
                    (funcall onsubmit
                             (flet ((str->option (str)
                                      (car (nth (parse-integer str) options))))
                               (if multiple
                                   (mapcar #'str->option (val-iter))
                                   (str->option (val-iter)))))))

    (let ((name (funcall name-iter)))
      (values
       `(select (name ,name ,@(when multiple `(multiple t)) ,@select-attrs)
                ,(loop for (option-val option-text) in options
                    for i from 0
                    for selected = (member option-val (if multiple val (list val)))
                    collect `(option (value ,i label ,option-text
                                            ,@(when selected
                                                `(selected t))
                                            ,@option-attrs))))
       name))))

(defun button (text html-attrs)
  (conformlet (:name-iter name-iter :extra-args (onclick))

    (custom-event (:val-iter val-iter)
                  (when (funcall val-iter)
                    (funcall onclick)))

    `(button (name ,(funcall name-iter) type "submit" ,@html-attrs)
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
  ;; onsubmit called only when validation passes
  (conformlet (:val val :onsubmit onsubmit)
    (multiple-value-bind (input-html name)
        (conform input-conformlet
                 :val val
                 :onsubmit (lambda (new-val)
                             (if (funcall validate new-val)
                                 (funcall onsubmit new-val)
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
                               &key (validate (constantly t)) (error "Password validation failed")
                                 (confirm-error "Passwords did not match"))
  (conformlet (:val val)
    (let (pw1)

      `(,(conform (string-field label-1 :validate validate :error error :type "password")
                  :val ""
                  :onsubmit (lambda (new-val)
                              (setf pw1 new-val)))
         ,(conform (string-field label-2 :validate (lambda (new-val)
                                                     ;; don't give confirmation error if pw1 failed.
                                                     (or (not pw1) (equal pw1 new-val)))
                                 :error confirm-error
                                 :type "password")
                   :val ""
                   :onsubmit (lambda (new-val)
                               (declare (ignore new-val))
                               (when pw1
                                 (setf val pw1))))))))

(defgeneric default-conformlet (val)
  (:documentation "Return a default conformlet for the value. See auto-conformlet"))

(defun auto-conformlet ()
  (conformlet (:val val)
    (conform (default-conformlet val)
             :val val)))

