(in-package #:conform/examples)

(defvar *port* 4323)
(setq *field-classes* '("pure-control-group"))

;;;;;;;;;;;;;;;;;;;;
;;  EDIT PROFILE  ;;
;;;;;;;;;;;;;;;;;;;;

(defclass profile ()
  ((first-name :initform "")
   (last-name :initform "")
   (password :initform "password")
   (date-of-birth :initform 0)))

(defvar *profile* (make-instance 'profile))

(defun profile-conformlet ()
  (conformlet (:val profile)
    (with-slots (first-name last-name password date-of-birth)
        profile

      `(div (class "profile-editor")

            (h2 () "Profile details")
            ,(conform (string-field "First Name" :placeholder "John")
                      :val first-name)
            ,(conform (string-field "Last Name" :placeholder "Doe")
                      :val last-name)
            ;; built-in date field automatically converts to Lispy universal-time, and adds a
            ;; yyyy-mm-dd placeholder if date fields are not supported.
            ;; ,(conform (date-field "Date of Birth")
            ;;           :val date-of-birth)

            (h2 () "Change Password")
            ;; the confirm-password-field does not display the passed-in :val -- it only updates it,
            ;; an only when the fields are equal.
            ,(conform (confirm-password-field "New Password" "Confirm Password"
                                              :validate (lambda (new-val)
                                                          (member #\? (coerce new-val 'list)))
                                              :error "Password must contain a question mark"
                                              :confirm-error "Enter the same password, thumbass!")
                      :val password)))))

(hunchentoot:define-easy-handler (profile-handler :uri "/profile") ()
  (html-document->string
   `(form (method "POST" action "" class "pure-form pure-form-aligned")
          ,(render-form "edit_profile" #'hunchentoot:post-parameter
                        (conformlet ()
                          `(,@(when *form-errors*
                                `((div (class "form-errors")
                                       ,(loop for err in *form-errors*
                                           collect `(div (style "color: red") ,err)))))
                              ,(conform (profile-conformlet) :val *profile*))))
          (div (class "pure-controls")
               (button (type "submit") "Save Profile")))))

(defun start (port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
