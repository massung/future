;;;; Futures/Promises for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
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
  (:use :cl :lw :mp :capi)
  (:export
   #:with-promise

   ;; promises
   #:promise
   #:promise-deliver
   #:promise-get

   ;; futures
   #:future
   #:future-promise
   #:future-realized-p
   #:future-join))

(in-package :future)

(defmacro with-promise ((var form &key apply-in-pane-process (errorp t) error-value) &body body)
  "Create a future, wait for it in another thread, then execute body with the promised value."
  (let ((f (gensym "future"))
        (c (gensym "consumer")))
    `(flet ((,c (,f)
              (let ((,var (future-join ,f :errorp ,errorp :error-value ,error-value)))
                ,(if apply-in-pane-process
                     `(apply-in-pane-process-if-alive ,apply-in-pane-process #'(lambda () ,@body))
                   `(progn ,@body)))))
       (prog1 nil
         (process-run-function "Promise" nil #',c (future ,form))))))

(defclass promise ()
  ((value     :reader promise-value    :initform :no-value))
  (:documentation "A container for an immutable, future-delivered value."))

(defmethod promise-deliver ((promise promise) value)
  "If the promise hasn't yet been set, set it now."
  (sys:compare-and-swap (slot-value promise 'value) :no-value value))

(defmethod promise-get ((promise promise) &key timeout)
  "Block until the promise has been delivered and return the value."
  (flet ((delivered-p ()
           (not (sys:compare-and-swap (slot-value promise 'value) :no-value :no-value))))
    (when (process-wait-with-timeout "Waiting for promise" timeout #'delivered-p)
      (promise-value promise))))

(defmacro future (form)
  "Create a future object that wraps a process evaluating a form."
  `(make-instance 'future :function #'(lambda () ,form)))

(defclass future ()
  ((promise   :reader future-promise   :initform (make-instance 'promise))
   (condition :reader future-condition :initform nil)
   (process   :reader future-process   :initform nil))
  (:extra-initargs '(:function :args))
  (:documentation "A deferred evaluation."))

(defmethod print-object ((future future) stream)
  "Output a future to a stream."
  (print-unreadable-object (future stream :type t)
    (if (future-realized-p future)
        (if-let (c (future-condition future))
            (format stream "ERROR ~s" (princ-to-string c))
          (format stream "OK ~s" (promise-get (future-promise future))))
      (princ "UNREALIZED" stream))))

(defmethod initialize-instance :after ((future future) &key function args)
  "Start the future process."
  (with-slots (promise condition process)
      future
    (flet ((producer ()
             (handler-case
                 (promise-deliver promise (apply function args))
               (condition (c)
                 (sys:atomic-exchange condition c)))))
      (setf process (process-run-function "Future" nil #'producer)))))

(defmethod future-realized-p ((future future))
  "T if the future's producer has finished executing."
  (not (process-alive-p (future-process future))))

(defmethod future-join ((future future) &key timeout (errorp t) error-value)
  "Wait for a future to be realized and then return its value or signal its condition."
  (when (process-join (future-process future) :timeout timeout)
    (if-let (c (future-condition future))
        (if errorp
            (error c)
          (values error-value t))
      (values (promise-get (future-promise future)) t))))
