# Futures and Promises for LispWorks

The `future` package is an implementation of the [futures and promises](http://en.wikipedia.org/wiki/Futures_and_promises) pattern for [LispWorks](http://www.lispworks.com).

Unlike many other futures and promises implementations, futures and promises here are *not the same!*. Promises are used by futures and are a lower-level construct.

## Promises

Promises are a first-class, atomic, immutable object. They can be **delivered once** and read many times. They are a very lightweight (atomic) method of communicating a single value across process without resorting to mailboxes or conditional locks.

For example:

	CL-USER > (setf p (make-instance 'promise))
	#<PROMISE 20094A97>
	
	CL-USER > (promise-deliver p 42)
	T
	
	CL-USER > (promise-get p)
	42

Once delivered, any subsequent calls to `promise-deliver` will return `nil`.

If `promise-get` is called before the promise has been delivered, then the process will block until the promise has been delivered.

	CL-USER > (setf p (make-instance 'promise))
	#<PROMISE 3028BE6F>
	
	CL-USER > (mp:process-run-function "" nil #'(lambda () (mp:current-process-pause 10) (promise-deliver p 'done)))
	#<MP:PROCESS Name "" Priority 0 State "Paused">
	
	CL-USER > (promise-get p)
	DONE
	
## Futures

A future is a first-class object that wraps a promise and process that will deliver a value to the promise.

Futures are created with a function and list of arguments. The result of the function will be the value delivered to the promise.

	CL-USER > (make-instance 'future :function #'+ :args '(1 2))
	#<FUTURE UNREALIZED>
	
	CL-USER > *
	#<FUTURE OK 3>

As a helper, the `future` macro can be used to create a future for a single form.

	CL-USER > (setf f (future (+ 1 2)))
	#<FUTURE OK 3>

You can test to see if a future has been realized with `future-realized-p`.

	CL-USER > (future-realized-p f)
	T
	
You can use `future-join` to wait until the producer process has completed and the future's promise has been delivered.

	CL-USER > (future-join f :timeout 10)
	3
	T

By default, *timeout* is `nil`, which will block forever, otherwise the *timeout* is in seconds. If the promise is delivered before the timeout expires, the return values will be the delivered value and `T`. If the timeout expires the second value will be `nil`.

If the future process signals a condition, the condition is caught by the future and `future-join` will re-signal the error in the current process.

	CL-USER > (future (+ 1 'a))
	#<FUTURE ERROR "In + of (1 A) arguments should be of type NUMBER.">
	
	CL-USER > (future-join *)
	Error: In + of (1 A) arguments should be of type NUMBER.
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.

## Producer, Meet Consumer...

As a helpful wrapper, the `with-promise` macro can be used to both produce a promised value and wait for/consume it in a separate thread.

	(with-promise (var form &key apply-in-pane-process) &body body)

For example:

	CL-USER > (with-promise (x (+ 1 2))
	            (capi:display-message (princ-to-string x)))

The *apply-in-pane-process* argument - if provided - should be a CAPI pane object. The *body* will be executed in that pane's process instead of the same process that's consuming the promise.
