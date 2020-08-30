;;;; conform.lisp

(in-package #:conform)

;;;;;;;;;;;;;;;;;
;;  FRAMEWORK  ;;
;;;;;;;;;;;;;;;;;

(defmacro conformlet ((&key ((:val val-var))
                            (order 0)
                            ((:name-iter name-iter-var) (gensym "NAME-ITER"))
                            extra-args)
                      &body body)
  ;; children: ((event-fn default-order explicit-order) (event-fn-2 do2 eo2))
  (once-only (order)
    (with-gensyms (children-var onsubmit-var)
      ;; parenthesis hell?
      `(lambda (&key
                  ,@(when val-var
                      `(((:val ,val-var))
                        ((:onsubmit ,onsubmit-var))))
                  ,@extra-args
                &aux ,children-var)

         ;; check data instance parameters provided when this is a data conformlet
         ,@(when val-var
             `((declare (function ,onsubmit-var))
               (assert ,val-var (,val-var)
                       "Data conformlets require the :val instance argument")
               (assert ,onsubmit-var (,onsubmit-var)
                       "Data conformlets require the :onsubmit instance argument")))

         (list
          (lambda (,name-iter-var)
            (flet
                ((conformlet-raw (order conformlet-instance)
                   (declare (cons conformlet-instance)
                            ((or number (member :epilogue)) order))
                   ;; register the event handler... (cdr formlet) => (event-handler default-order)
                   (push (cons (cdr conformlet-instance) order) ,children-var)
                   ;; ...and actually render the body
                   (funcall (car conformlet-instance) ,name-iter-var)))

              (macrolet
                  ((conformlet (conformlet
                                   &rest args
                                   &key data onsubmit (order 0)
                                   &allow-other-keys
                                   &environment env
                                   ;; TODO: figure out conflict between alexandria and metatilities
                                   &aux (extra-args (remove-from-plist args :data :onsubmit :order)))
                     ;; we do /not/ once-only on extra-args. once-only'ing the whole list (with the
                     ;; /forms/ for each value) is not useful -- we'd need to once-only each value,
                     ;; which is more trouble than just being careful not to eval it twice.
                     (once-only (onsubmit order conformlet)
                       (if data
                           ;; don't need gensyms -- no user code eval'd in this lexical context
                           (multiple-value-bind (vars vals stores writer reader)
                               (get-setf-expansion data env)
                             ;; TODO: declarations to check type of conformlet
                             `(let* (,@(mapcar #'list vars vals)
                                     (read-value ,reader))
                                (conformlet-raw
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
                           `(conformlet-raw ,order (funcall ,conformlet ,@extra-args)))))

                   (custom-event ((&key (order :epilogue)
                                        ((:val-iter val-iter-var) (gensym "VAL-ITER") val-iter-p))
                                  &body body)
                     (once-only (order)
                       `(conformlet-raw ,order (list (constantly nil) ; no render function
                                                     (lambda (,val-iter-var)
                                                       ,@(unless val-iter-p
                                                           `((declare (ignore ,val-iter-var))))
                                                       ,@body) ; event fn
                                                     0)))))                          ; default order

                ,@(if val-var
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
(defun render-form (prefix post-getter conformlet &rest instance-args &aux *form-errors*)
  "post-getter is a function that, given a string, gets the corresponding POST value (eg,
#'hunchentoot:post-parameter). name-prefix, if provided, is used to distinguish fields of this form
from fields of other forms in the same HTML page."
  (destructuring-bind (render handle-events) (apply conformlet instance-args)
    (funcall render (make-name-iterator prefix))
    (funcall handle-events (compose post-getter (make-name-iterator prefix)))
    (funcall render (make-name-iterator prefix))))

;;;;;;;;;;;;;;
;;  FIELDS  ;;
;;;;;;;;;;;;;;

;; TODO: labels for all of them

(defun string-input (html-attrs)
  (conformlet (:val val :name-iter name-iter)

              (custom-event (:val-iter val-iter)
               (setf val (funcall val-iter)))

              `(input (name ,(funcall name-iter) ,@html-attrs))))

(defun select-input (options select-attrs option-attrs &optional multiple)
  (conformlet (:val val :name-iter name-iter)

              (custom-event (:val-iter val-iter)
                            ;; TODO: error handling for parse integer?
                            (flet ((str->option (str)
                                     (car (nth (parse-integer str)) options)))
                              (if multiple
                                  (mapcar #'str->option (val-iter))
                                  (str->option (val-iter)))))

              `(select (name ,(name-iter) ,@(when multiple `(multiple t)) ,@select-attrs)
                       ,(loop for (option-key option-text) in options
                           collect `(option (value ,option-val ,@option-attrs) ,option-text)))))
