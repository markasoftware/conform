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

(defclass profile ()
  ((first-name :initform "")
   (last-name :initform "")
   (password :initform "password")
   (accepted-tos :initform nil)))

(defun profile-conformlet (profile)
  (conformlet (:places profile)
    (with-slots (first-name last-name password accepted-tos gender)
        profile

      `(div (class "profile-editor")

            (h2 () "Profile details")
            ,(string-field (place first-name) "First Name" :placeholder "John")
            ,(string-field (place last-name) "Last Name" :placeholder "Doe")
            ,(checkbox-field (place accepted-tos) "Accepted Terms of Service")
            ;; ,(radio-field (place gender) '((:male "Male")
            ;;                                (:female "Female")
            ;;                                (:classified "Classified"))
            ;;               "Gender")

            (h2 () "Change Password")
            ;; the confirm-password-field does not display the passed-in :val -- it only updates it,
            ;; an only when the fields are equal.
            ,(confirm-password-field (place password)
                                     "New Password" "Confirm Password"
                                     :validate (lambda (new-val)
                                                 (member #\? (coerce new-val 'list)))
                                     :error "Password must contain a question mark"
                                     :confirm-error "Enter the same password, thumbass!"
                                     :suppress-error-when-empty t)))))

(defun advanced-list (val subconformlet make-default)
  (declare (function subconformlet make-default))
  (conformlet (:places val)
    `(div (class "form-list")
          ,(loop for i from 0 below (length val)
              collect (let ((k i))      ; capture the value permanently for closures

                        `(div (style "border-bottom: 2px solid gray; padding: 1rem 0;")

                              ,(funcall subconformlet (place (nth k val)))

                              (div (class "pure-controls")
                                   ,(button (lambda () (delete-item-at val k)) "Delete")
                                   ,(when (> k 0)
                                      (button (lambda ()
                                                (swapcar (nthcdr (1- k) val)
                                                         (nthcdr k val)))
                                              "Move up"))
                                   ,(when (< k (1- (length val)))
                                      (button (lambda ()
                                                (swapcar (nthcdr k val)
                                                         (nthcdr (1+ k) val)))
                                              "Move down"))))))
          (div (class "pure-controls")
               ,(button (lambda ()
                          (appendf val (list (funcall make-default))))
                        "Add new")
               ,(button (lambda ()
                          (setf val (shuffle val)))
                        "Shuffle")
               ,(button (lambda ()
                          (setf val (nthcdr (ceiling (length val) 2) (shuffle val))))
                        "Thanos")))))

(defun cons-tree (val)
  "Conformlet for a cons tree of strings or integers."
  (labels ((cell-type-selector (onclick)
             (conformlet (:order 1)
               (let (new-val)
                 `(
                   ;; because we call select-input anew for each cell-type-selector conformlet
                   ;; instantiated, there's no chance of the same cons cell being re-used in multiple
                   ;; places causing unexpected linkages
                   ,(select-input (place new-val)
                                  `((nil "NIL")
                                    ("" "String")
                                    (,(cons nil nil) "Cons")))
                   (br)
                   ,(button
                     (lambda ()
                       (funcall onclick new-val))
                     "Change")))))
           (cons-cell (val)
             (conformlet (:places val)
               `((td ()
                     ,(cell-type-selector
                       (lambda (new-val)
                         (setf val new-val))))
                 (td ()
                     ,(etypecase val
                        (cons (cons-tree (place val)))
                        (string (string-input (place val)))
                        (null "NIL")))))))

    (conformlet (:places val)
      `(table (class "cons-tree" style "border: 2px solid gray; margin-left: 1rem;")
              (tr ()
                  (td ()  "CAR: ") ,(cons-cell (place (car val))))
              (tr ()
                  (td () "CDR: ") ,(cons-cell (place (cdr val))))))))

