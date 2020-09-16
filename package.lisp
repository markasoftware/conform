;;;; Copyright 2020 Mark Polyakov
;;;; Released under the MIT license

(defpackage #:conform
  (:use #:cl #:alexandria)
  (:export

   #:place
   #:with-places

   #:conformlet
   #:render-form
   #:custom-event

   #:string-input
   #:select-input
   #:checkbox-input
   #:radio-input
   #:button
   #:string-field
   #:select-field
   #:checkbox-field
   #:radio-field
   #:confirm-password-field

   #:*form-errors*
   #:*field-classes*
   #:*string-classes*
   #:*select-classes*
   #:*radio-container-classes*
   #:*radio-label-classes*
   ))
