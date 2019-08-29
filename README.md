# Lack, the core of Clack

[![Build Status](https://travis-ci.org/fukamachi/lack.svg?branch=master)](https://travis-ci.org/fukamachi/lack)
[![Coverage Status](https://coveralls.io/repos/fukamachi/lack/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/lack)

Lack is a Common Lisp library which allows web applications to be constructed of modular components. It was originally a part of [Clack](https://github.com/fukamachi/clack), however it's going to be rewritten as an individual project since Clack v2 with performance and simplicity in mind.

The scope is defining Lack applications and wrapping it up with Lack middlewares. On the other hand, [Clack](https://github.com/fukamachi/clack) is an abstraction layer for HTTP and HTTP servers and provides unified API.

## Warning

This software is still BETA quality. The APIs are being finalized.

## Usage

```common-lisp
(defparameter *app*
  (lambda (env)
    '(200 (:content-type "text/plain") ("Hello, World"))))

;; `wrap` the app with middleware
(setf *app* (funcall lack-middleware-session:*lack-middleware-session* *app*))

;; to wrap with multiple middlewares at once, use lack's builder macro
(setf *app*
  (lack:builder
    :session
    (:static :path "/public/"
             :root #P"/static-files/")
    (lambda (app)
      (lambda (env)
        (prog1 (funcall app env)
          (do-before-responding))))
    *app*))
```

Use Clack's `clackup` for starting a Lack application.

```common-lisp
(clack:clackup *app* :server :woo)
```

## The Environment

The environment, an application takes, is a property list containing the following keys:

- `:request-method` (Required, Keyword)
  - The HTTP request method: `:GET`, `:HEAD`, `:OPTIONS`, `:PUT`, `:POST`, or `:DELETE`.
- `:script-name` (Required, String)
  - The initial portion of the request URI path that corresponds to the Clack application. The value of this key may be an empty string when the client is accessing the application represented by the server's root URI. Otherwise, it is a non-empty string starting with a forward slash (`/`).
- `:path-info` (Required, String)
  - The remainder of the request URI path. The value of this key may be an empty string when you access the application represented by the server's root URI with no trailing slash.
- `:query-string` (Optional, String)
  - The portion of the request URI that follows the `?`, if any.
- `:url-scheme` (Required, String)
  - `"http"` or `"https"`, depending on the request URI.
- `:server-name` (Required, String)
  - The resolved server name or the server IP address.
- `:server-port` (Required, Integer)
  - The port on which the request is being handled.
- `:server-protocol` (Required, Keyword)
  - The version of the protocol the client used to send the request: typically `:HTTP/1.0` or `:HTTP/1.1`.
- `:request-uri` (Required, String)
  - The request URI. Always starts with "/".
- `:raw-body` (Optional, Stream)
  - The new body of the request.
- `:remote-addr` (Required, String)
  - The remote address.
- `:remote-port` (Required, Integer)
  - The remote port.
- `:content-type` (Optional, String)
  - The header value of Content-Type.
- `:content-length` (Optional, Integer)
  - The header value of Content-Length.
- `:headers` (Required, Hash-Table)
  - A hash table of headers.

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

The response body must be returned from the application in one of three formats, a list of strings, a byte vectors, or a pathname.

### Delayed Response and Streaming Body

Lack allows applications to provide a callback-style response instead of the three-element list. This allows for a delayed response and a streaming body.

To enable a delayed response, the application should return a callback as its response.

```common-lisp
(lambda (env)
  (lambda (responder)
    (let ((content (fetch-something)))
      (funcall responder `(200 (:content-type "text/plain") (,content))))))
```

An application may omit the third element (the body) when calling the responder. If the body is omitted, the responder will return a function which takes a body chunk, and optional `:start`, `:end` and `:close` keyword arguments.

```common-lisp
(lambda (env)
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "application/json")))))
      (loop for chunk = (fetch-something)
            do (funcall writer chunk :close (null chunk))
            while chunk))))
```

In case of that you would prefer a stream to a function, `lack.util.writer-stream` wraps the function and allows you to treat it as a stream:

```common-lisp
(import 'lack.util.writer-stream:make-writer-stream)

(lambda (env)
  (lambda (responder)
    (let* ((writer (funcall responder '(200 (:content-type "application/json"))))
           (stream (make-writer-function writer)))
      (loop for chunk = (fetch-something)
            do (write-sequence chunk stream)
            while chunk
            finally
              (finish-output stream)))))
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
* Lack.Middleware.Auth.Basic
* Lack.Middleware.Backtrace
* Lack.Middleware.Csrf
* Lack.Middleware.Mount
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

## Benchmark

|  Hunchentoot  |  Clack  |  Lack   |
|---------------|---------|---------|
|    3384.15    | 3896.51 | 4252.68 |

Lack is 1.25 times faster than Hunchentoot and 1.1 times faster than Clack.

* MacBook Pro Retina, 13-inch, Early 2013 (CPU: 3GHz Intel Core i7 / Memory: 8GB 1600 MHz)
* SBCL 1.2.6
* wrk 3.1.1
* Hunchentoot 1.2.29

You can get the benchmark code at "[benchmark/](https://github.com/fukamachi/lack/tree/master/benchmark)".

### Hunchentoot

```
wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     1.32ms    8.92ms  78.41ms   98.62%
    Req/Sec     3.59k     0.93k    5.55k    73.51%
  33857 requests in 10.00s, 7.62MB read
  Socket errors: connect 0, read 0, write 0, timeout 33
Requests/sec:   3384.15
Transfer/sec:    779.94KB
```

### Clack

```common-lisp
wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.52ms   23.39ms 170.24ms   98.07%
    Req/Sec     4.13k   768.76     4.67k    95.03%
  38996 requests in 10.01s, 10.12MB read
  Socket errors: connect 0, read 0, write 0, timeout 33
Requests/sec:   3896.51
Transfer/sec:      1.01MB
```

### Lack

```common-lisp
wrk -c 10 -t 4 -d 10 http://127.0.0.1:5000
Running 10s test @ http://127.0.0.1:5000
  4 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency     3.88ms   25.06ms 175.76ms   97.92%
    Req/Sec     4.52k   832.13     5.11k    94.92%
  42601 requests in 10.02s, 11.01MB read
  Socket errors: connect 0, read 0, write 0, timeout 33
Requests/sec:   4252.68
Transfer/sec:      1.10MB
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/lack/graphs/contributors)

## License

Licensed under the LLGPL License.
