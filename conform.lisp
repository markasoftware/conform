;;;; conform.lisp

(in-package #:conform)

;;;;;;;;;;;;;;;;;
;;  UTILITIES  ;;
;;;;;;;;;;;;;;;;;

(define-modify-macro push-end (item) (lambda (list item) (append list (list item))))

(defmacro assocf (place item default-cdr &key (test #'eql) &environment env)
  "Try to access and return the element of the alist at place corresponding to the key, inserting
the default cdr and returning the corresponding cons if nothing is present."
  (multiple-value-bind (vals vars stores writer reader)
      (get-setf-expansion place env)
    (once-only (item default-cdr test)
      `(let ,(mapcar #'list vals vars)
         (if-let ((result (assoc ,item ,reader :test ,test)))
           result
           (let* ((result (cons ,item ,default-cdr))
                  (,(car stores) (cons result ,reader)))
             ,writer
             result))))))

;;;;;;;;;;;;;;;;;
;;  FRAMEWORK  ;;
;;;;;;;;;;;;;;;;;

(defmacro with-conform ((&key (id-strategy :smart)) render-form &optional after-events-form)
  ""
  (declare ((member :smart :sequential :random) id-strategy))
  (with-gensyms (children-var eql-trans-var gensym-sequence-var equal-fuck-var name-iter-var
                              gensym-counter-var next-gensym-fun)
    `(let ((,children-var)
           (,eql-trans-var)
           (,gensym-sequence-var)
           (,equal-fuck-var))
       (cons
        (lambda (,name-iter-var)
          (let ((,gensym-counter-var 0))
            (flet
                ((conformlet-raw (id order conformlet &rest conformlet-args)
                   (declare (function conformlet)
                            (number order))
                   (let ((existing-instance (third (assoc id ,children-var))))
                     (if existing-instance
                         (funcall (car existing-instance) ,name-iter-var)
                         (let ((new-instance (funcall conformlet)))
                           (push (list id order new-instance) ,children-var)
                           (funcall (car new-instance) ,name-iter-var)))))
                 (,next-gensym-fun (&aux (g (gensym)))
                   (or (nth (incf ,gensym-counter-var) ,gensym-sequence-var)
                       (progn
                         (push-end ,gensym-sequence-var g)
                         g))))
              (macrolet
                  ((conformlet (conformlet
                                &rest args
                                &key data onsubmit (id nil id-p) (order 0)
                                &allow-other-keys
                                &environment env
                                &aux extra-args (remove-from-plist args :data :onsubmit :id :order))
                     (once-only (id onsubmit order)
                       (multiple-value-bind (vars vals stores writer reader)
                           (get-setf-expansion data env)
                         `(let* (,@(mapcar #'list vars vals)
                                 (read-value ,reader))
                            (conformlet-raw
                             ,comformlet
                             ;; TODO: default to a number or something if data is not provided.
                             ,(if id-p
                                  id
                                  (ecase id-strategy
                                    (:smart
                                     `(cdr
                                       (assocf ,',equal-fuck-var
                                               )))
                                    (:sequential
                                     `(,',next-gensym-fun))
                                    (:random
                                     `(gensym))))
                             ,order
                             (append (list ,reader ,(or onsubmit
                                                        `(lambda (v)
                                                           (push (cons v read-value) ,eql-trans-var)
                                                           (let ((,',(car stores) v))
                                                             ,',writer))))
                                     extra-args)))))))
 
                ,render-form))))
        (lambda (val-iter)
          ;; most recent child is first at the beginning
          (dolist (child (reverse (stable-sort ,children-var #'> :key #'second)))
            (funcall (cdr child) val-iter))
          ,after-events-form)))))

(defun make-name-iterator (prefix)
  (let ((i 0))
    (lambda ()
      (format nil "~a~a" prefix (incf i)))))

(defvar *unique-form-id* 0)

;; TODO: custom name for the form
(defun render-whole-form (instantiated-conformlet post-getter
                          &optional (prefix (format nil "conform~a" (incf *unique-form-id*))))
  "instantiated-conformlet should be the result of a with-conform call. post-getter is a function
that, given a string, gets the corresponding POST value (eg, #'hunchentoot:post-parameter).
name-prefix, if provided, is used to distinguish fields of this form from fields of other forms in
the same HTML page."
  (declare (cons instantiated-conformlet)
           (function post-getter)
           (string name-prefix))
  (destructuring-bind (render . handle-events) instantiated-conformlet
    (funcall render (make-name-iterator prefix))
    (funcall handle-events (compose post-getter (make-name-iterator prefix)))
    (funcall render (make-name-iterator prefix))))

(defmacro defconformlet (name (val-var &rest args) &body body)
  (with-gensyms (id-var body-var)
    `(defmacro ,name (,id-var ,val-var ,@args)
       ;; if we're in a with-formite
       `(formify-child ,,id-var ,,val-var
                       ;; TODO: do this without a lambda? Body isn't being executed in its original
                       ;; lexical environment anyway.
                       (lambda ()
                         ,@,body)))))

(defmacro defconformlet-raw (name configurator-lambda-list render-form event-form)
  `(defmacro name (&rest args)
     "If in a with-conform body, instantiate a child conformlet"))

(defmacro defconformlet-raw-data (name configurator-lambda-list val-var &body body)
  (let* ((documentation (if (stringp (car body))
                            (car body)
                            "No conformlet-specific documentation provided."))
         (body (if documentation (cdr body) body)))
    (with-gensyms (writer orig-body iter-var)
      `(defun ,name ,configurator-lambda-list
         ,(concatenate 'string
                       "If in a with-conform statement:

Instantiate this conformlet. The first argument should be a data designator, defined as such:
  + If it's a valid place, data is provided from and written back to that place.
  + If it's a list where the car is :manual then the cadr is a form that's expanded to determine the
    input data and the caddr is a function that will be called with the updated data from the
    conformlet. Many conformlets (including the built-in list one!) will modify the data passed in,
    so you cannot turn Conform into a functional framework by simply using :manual everywhere --
    you must make sure that the sub-conformlets do not mutate parts of the value.

Else:

Return a function that can be passed as the second argument to (birth) in a with-conform statement
later (the first argument should be a data designator, as defined above).

"
                       documentation)
         (lambda (,val-var ,writer)
           ;; wrap the event handler to call the writer with val
           (let ((,orig-body (progn ,@body)))
             (cons (car ,orig-body)
                   (lambda (,iter-var)
                     (funcall (cdr ,orig-body) ,iter-var)
                     (funcall ,writer ,val-var)))))))))

;;;;;;;;;;;;;;
;;  FIELDS  ;;
;;;;;;;;;;;;;;

;; TODO: labels for all of them

(defconformlet-raw-data string-field (&rest attrs) val
  (cons
   (lambda (name-iter)
     `(:input (:name ,(name-iter) ,@attrs)))
   (lambda (val-iter)
     (setf val (val-iter)))))

;; TODO: multi-select
(defconformlet-raw-data select-field (options &rest attrs) val
  "Each element of the list options should be a two-element proper list, where the first option is
an identifier that will be the returned value, and the second value is the text that should be
displayed to the user. Alternately, it may be a single element, which will be stringified"
  (cons
   (lambda (name-iter)
     `(:select (:name ,(name-iter))
               ,(loop for i from 0
                   for option in options
                   ;; TODO: customizeable :test
                   collect `(:option (:value ,i
                                             ,@(when (equal (car (ensure-list option)) val)
                                                 '(:selected t))
                                             ,@attrs)
                                     ,(if (listp option)
                                          (cadr option)
                                          (string option))))))
   (lambda (val-iter)
     (setf val (car (ensure-list
                     (nth (parse-integer (val-iter)) options)))))))

