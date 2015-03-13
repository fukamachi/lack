# Lack, a minimal Clack

[![Build Status](https://travis-ci.org/fukamachi/lack.svg?branch=master)](https://travis-ci.org/fukamachi/lack)

Lack is an experimental project for redesigning Clack with performance and simplicity in mind. This aims to be a gut of the next Clack major release.

## Usage

```common-lisp
(defvar *app*
  (lambda (env)
    '(200 (:content-type "text/plain") ("Hello, World"))))

(lack:lackup *app* :server :woo)

;; `wrap`
(funcall lack-middleware-session:*lack-middleware-session* *app*)

(lack:builder
  :session
  (:static :path "/public/"
           :root #P"/static-files/")
  (lambda (app)
    (lambda (env)
      (prog1 (funcall app env)
        (do-before-responding))))
  *app*)
```

## The Environment

The environment, an application takes, is a property list containing the following keys:

- `:method` (Required, Keyword)
  - The HTTP request method: `:GET`, `:HEAD`, `:OPTIONS`, `:PUT`, `:POST`, or `:DELETE`.
- `:script-name` (Required, String)
  - The initial portion of the request URI path that corresponds to the Clack application. The value of this key may be an empty string when the client is accessing the application represented by the server's root URI. Otherwise, it is a non-empty string starting with a forward slash (`/`).
- `:path-info` (Required, String)
  - The remainder of the request URI path. The value of this key may be an empty string when you access the application represented by the server’s root URI with no trailing slash.
- `:query-string` (Optional, String)
  - The portion of the request URI that follows the `?`, if any.
- `:server-name` (Required, String)
  - The resolved server name or the server IP address.
- `:server-port` (Required, Integer)
  - The port on which the request is being handled.
- `:server-protocol` (Required, Keyword)
  - The version of the protocol the client used to send the request: typically `:HTTP/1.0` or `:HTTP/1.1`.
- `:uri` (Required, String)
  - The request URI. Always starts with "/".
- `:raw-body` (Optional, Stream)
  - The new body of the request.
- `:remote-addr` (Required, String)
  - The remote address.
- `:remote-port` (Required, Integer)
  - The remote port.
- `:headers` (Required, Hash-Table)
  - A hash table of headers.

Old versions of Clack also had the following keys. These are now deprecated.

- `:request-method`
  - Use `:method` instead.
- `:request-uri`
  - Use `:uri` instead.

## The Response

### Normal response

An application returns a list of three elements for a normal request, which respectively expresses an HTTP status code, headers, and response body data.

```common-lisp
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, World")))
```

The status code must be an integer greater than or equal to 100, and should be an HTTP status code as documented in [RFC 2616](https://www.ietf.org/rfc/rfc2616.txt).

The headers must be a property list. If the same key name appears multiple times in it, those header lines will be sent to the client separately (e.g. multiple `Set-Cookie` lines).

The response body must be returned from the application in one of three formats, a list of strings, a list of byte vectors, or a pathname.

### Delayed Response and Streaming Body

Lack allows applications to provide a callback-style response instead of the three-element list. This allows for a delayed response and a streaming body.

To enable a delayed response, the application should return a callback as its response.

```common-lisp
(lambda (env)
  (lambda (responder)
    (let ((content (fetch-something)))
      (funcall responder `(200 (:content-type "text/plain") (,content))))))
```

An application may omit the third element (the body) when calling the responder. If the body is omitted, the responder will return a function which takes a body chunk and `:close` keyword argument.

```common-lisp
(lambda (env)
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "application/json")))))
      (loop for chunk = (fetch-something)
            do (funcall writer chunk :close (null chunk))
            while chunk))))
```

This delayed response and streaming API is useful if you want to implement a non-blocking I/O based server streaming or long-poll Comet push technology.

## Middlewares

Lack middleware is a component wrapping an application. It is a function which takes an application and returns a new application.

```common-lisp
(defvar *mw*
  (lambda (app)
    (lambda (env)
      ;; preprocessing
      (let ((res (funcall app env)))
        ;; postprocessing
        res))))

;; getting a wrapped app
(funcall *mw* *app*)
```

Lack provides some bundle middlewares.

* Lack.Middleware.Accesslog
* Lack.Middleware.Backtrace
* Lack.Middleware.Session
* Lack.Middleware.Static

```common-lisp
;; Using Lack.Middleware.Accesslog
(funcall lack.middleware.accesslog:*lack-middleware-accesslog*
         *app*)
```

### Using Lack.Builder

Lack.Builder gives you a quick DSL to wrap your application with Lack middlewares.

```common-lisp
(lack:builder
 (:static :path (lambda (path)
                  (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
                      path
                      nil))
          :root *static-directory*)
 :accesslog
 :session
 :backtrace
 (lambda (env)
   (declare (ignore env))
   '(200 () ("Hello, World"))))
```

It takes a list of middlewares and an app at the last.

```
builder middleware* app

middleware ::= keyword
             | null
             | symbol
             | function
             | (keyword arg*)
             | (symbol arg*)
             | normal-form

app ::= function
```

Typical builder syntax is like this:

```common-lisp
(lack:builder
  :foo
  (:bar :opt "val")
  *app*)
```

is syntactically equal to:

```common-lisp
(funcall lack.middleware.foo:*lack-middleware-foo*
         (funcall lack.middleware:bar:*lack-middleware-bar*
                  *app*
                  :opt "val"))
```

### Inline middleware

```common-lisp
(lack:builder
  (lambda (app)
    (lambda (env)
      ;; preprocessing
      (let ((res (funcall app env)))
        ;; postprocessing
        res)))
  *app*)
```

### Conditional middleware

```common-lisp
(lack:builder
  (if (productionp)
      nil
      :accesslog)
  (if *error-log*
      `(:backtrace :output ,*error-log*)
      nil)
  :session
  *app*)
```

## Using Lack in an existing Clack app

Just replace `clack.builder:builder` by `lack:builder`, a superset of `clack.builder:builder`.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
