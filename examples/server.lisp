;;;; Copyright 2020 Mark Polyakov
;;;; Released under the MIT license

(in-package #:conform/examples)

(defvar *port* 4323)
(setq *field-classes* '("pure-control-group")
      *radio-label-classes* '("pure-radio")

      hunchentoot:*show-lisp-errors-p* t)

;;;;;;;;;;;;;;;;;;;;
;;  EDIT PROFILE  ;;
;;;;;;;;;;;;;;;;;;;;

(defvar *string* "")
(defvar *all-profiles* nil)
(defvar *profile* (make-instance 'profile))
(defvar *cons* (cons nil nil))

(defun display-form-errors ()
  (when *form-errors*
    `((div (class "form-errors")
           ,(loop for err in *form-errors*
               collect `(div (style "color: red") ,err))))))

(defun post-p ()
  "Return whether the current Hunchentoot request is POST"
  (eq (hunchentoot:request-method*) :post))

(hunchentoot:define-easy-handler (string-handler :uri "/string") ()
  (html-document->string
   `(form (method "POST" action "")
          ,(render-form "edit_string" #'hunchentoot:post-parameter (post-p)
             `(,(string-input (place *string*))
                (button (type "submit") "Save"))))))

(hunchentoot:define-easy-handler (profile-handler :uri "/profile") ()
  (html-document->string
   `(form (method "POST" action "" class "pure-form pure-form-aligned")
          ,(render-form "edit_profile" #'hunchentoot:post-parameter (post-p)
             `(,(display-form-errors)
                ,(profile-conformlet (place *profile*))))
          (div (class "pure-controls")
               (button (type "submit") "Save Profile")))))

(hunchentoot:define-easy-handler (admin-handler :uri "/admin") ()
  (html-document->string
   `(form (method "POST" action "" class "pure-form pure-form-aligned")
          ,(render-form "admin" #'hunchentoot:post-parameter (post-p)
             (list (display-form-errors)
                   (advanced-list (place *all-profiles*)
                                  #'profile-conformlet
                                  (curry #'make-instance 'profile))
                   `(div (class "pure-controls")
                         (button (type "submit") "Save all profiles")))))))

(hunchentoot:define-easy-handler (cons-handler :uri "/cons") ()
  (html-document->string
   `((form (method "POST" action "")
           ,(render-form "cons" #'hunchentoot:post-parameter #'post-p
              (conformlet ()
                `(,(cons-tree (place *cons*))
                   (button (type "submit") "Save All")))))
     (br)
     (br)
     ,(with-output-to-string (stream)
        (pprint *cons* stream)))))

(hunchentoot:define-easy-handler (home-handler :uri "/") ()
  (html-document->string
   `("Check out the following examples:"
     (ul ()
         (li () (a (href "/string") "Edit String") ": Simplest conformlet")
         (li () (a (href "/profile") "Edit Profile") ": Simple conformlet composition, no interactivity.")
         (li () (a (href "/admin") "Admin (Edit Multiple Profiles)") ": Interactive list conformlet.")
         (li () (a (href "/cons") "Cons Tree Editor") ": Recursive conformlet.")))))

(defun start (port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
