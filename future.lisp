;;;; Futures and Promises for ClozureCL
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;; All rights reserved.
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :future
  (:use :cl :ccl)
  (:export
   #:with-promise

   ;; promises
   #:promise
   #:promise-deliver
   #:promise-delivered-p
   #:promise-get

   ;; futures
   #:future
   #:future-condition
   #:future-join
   #:future-promise
   #:future-realized-p))

(in-package :future)

;;; ----------------------------------------------------

(defclass promise ()
  ((value :reader promise-value :initarg :value))
  (:documentation "A promised value."))

;;; ----------------------------------------------------

(defmethod promise-deliver ((p promise) value)
  "Deliver a value to a promise."
  (unless (promise-delivered-p p)
    (values (setf (slot-value p 'value) value) t)))

;;; ----------------------------------------------------

(defmethod promise-delivered-p ((p promise))
  "T if the promise has been delivered."
  (slot-boundp p 'value))

;;; ----------------------------------------------------

(defmethod promise-get ((p promise))
  "Wait until a promise has been delivered and return it value."
  (when (process-wait "Promise" #'promise-delivered-p p)
    (promise-value p)))

;;; ----------------------------------------------------

(defmethod print-object ((p promise) stream)
  "Output a promise to a stream."
  (print-unreadable-object (p stream :type t)
    (if (promise-delivered-p p)
        (format stream "DELIVERED ~s" (promise-value p))
      (princ "UNDELIEVERED" stream))))

;;; ----------------------------------------------------

(defclass future ()
  ((promise   :reader future-promise   :initform (make-instance 'promise))
   (condition :reader future-condition :initform ())
   (process   :reader future-process   :initform ()))
  (:documentation "A promise value producer."))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((f future) &key function &allow-other-keys)
  "Create a promise and spawn a producer process."
  (with-slots (promise condition process)
      f
    (flet ((producer ()
             (handler-case
                 (promise-deliver promise (funcall function))
               (condition (c)
                 (setf condition c)))))
      (setf process (process-run-function "Future" #'producer)))))

;;; ----------------------------------------------------

(defmethod future-realized-p ((f future))
  "T if the future's promise has been delivered or a condition signaled."
  (or (future-condition f) (promise-delivered-p (future-promise f))))

;;; ----------------------------------------------------

(defmethod future-join ((f future) &optional (errorp t) error-value)
  "Wait for a future's promise to be delievered."
  (when (join-process (future-process f))
    (let ((c (future-condition f)))
      (if c
          (if errorp
              (error c)
            (values error-value t))
        (values (promise-get (future-promise f)) t)))))

;;; ----------------------------------------------------

(defmethod print-object ((f future) stream)
  "Output a future toa  stream."
  (print-unreadable-object (f stream :type t)
    (cond ((future-condition f)
           (format stream "ERROR ~s" (princ-to-string (future-condition f))))
          ((future-realized-p f)
           (format stream "OK ~s" (promise-get (future-promise f))))
          (t
           (princ "UNREALIZED" stream)))))

;;; ----------------------------------------------------

(defmacro future (form)
  "Create a future from a form."
  `(make-instance 'future :function #'(lambda () ,form)))

;;; ----------------------------------------------------

(defmacro with-promise ((var form &optional (errorp t) error-value) &body body)
  "Create a future and consumer function that waits for the future."
  (let ((f (gensym "future"))
        (c (gensym "consumer")))
    `(flet ((,c (,f)
              (let ((,var (future-join ,f ,errorp ,error-value)))
                ,@body)))
       (prog1 nil
         (process-run-function "Promise" #',c (future ,form))))))
