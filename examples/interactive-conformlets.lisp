(in-package #:conform/examples)

(defun delete-item-at (list index)
  (declare (list list)
           (integer index))
  (if (zerop index)
      (cdr list)
      (let ((prevcdr (nthcdr (1- index) list)))
        (rplacd prevcdr (cddr prevcdr))
        list)))

(defun swapcar (cons1 cons2)
  (declare (cons cons1 cons2))
  (let ((temp (car cons1)))
    (rplaca cons1 (car cons2))
    (rplaca cons2 temp)))

(defun advanced-list (subconformlet make-default)
  (declare (function subconformlet make-default))
  (conformlet (:val val)
    `(div (class "form-list")
          ,(loop for i from 0 below (length val)
              collect (let ((k i)) ; capture the value permanently for closures

                        `(div (style "border-bottom: 2px solid gray; padding: 1rem 0;")

                              ,(conform subconformlet :val (nth k val))

                              (div (class "pure-controls")
                                   ,(conform (button "Delete")
                                             :onclick (lambda ()
                                                        (delete-item-at val k)))
                                   ,(when (> k 0)
                                      (conform (button "Move up")
                                               :onclick (lambda ()
                                                          (swapcar (nthcdr (1- k) val)
                                                                   (nthcdr k val)))))
                                   ,(when (< k (1- (length val)))
                                      (conform (button "Move down")
                                               :onclick (lambda ()
                                                          (swapcar (nthcdr k val)
                                                                   (nthcdr (1+ k) val)))))))))
          (div (class "pure-controls")
               ,(conform (button "Add new")
                         :onclick (lambda ()
                                    (appendf val (list (funcall make-default)))))
               ,(conform (button "Shuffle")
                         :onclick (lambda ()
                                    (setf val (shuffle val))))
               ,(conform (button "Thanos")
                         :onclick (lambda ()
                                    (setf val (nthcdr (ceiling (length val) 2) (shuffle val)))))))))

(defun cons-tree ()
  "Conformlet for a cons tree of strings or integers."
  (let* ((cell-type-selector
          (conformlet (:extra-args (onclick) :order 1)
            (let (new-val)
              `(
                ;; because we call select-input anew for each cell-type-selector conformlet
                ;; instantiated, there's no chance of the same cons cell being re-used in multiple
                ;; places causing unexpected linkages
                ,(conform (select-input `((nil "NIL")
                                          ("" "String")
                                          (,(cons nil nil) "Cons")))
                          :val new-val)
                (br)
                ,(conform (button "Change")
                          :onclick (lambda ()
                                     (funcall onclick new-val)))))))
         (cons-cell
          (conformlet (:val val)
            `((td ()
                  ,(conform cell-type-selector
                            :onclick (lambda (new-val)
                                       (setf val new-val))))
              (td ()
                  ,(etypecase val
                     (cons (conform (cons-tree) :val val))
                     (string (conform (string-input) :val val))
                     (null "NIL")))))))

    (conformlet (:val val)
      `(table (class "cons-tree" style "border: 2px solid gray; margin-left: 1rem;")
              (tr ()
                  (td ()  "CAR: ") (td () ,(conform cons-cell :val (car val))))
              (tr ()
                  (td () "CDR: ") (td () ,(conform cons-cell :val (cdr val))))))))
