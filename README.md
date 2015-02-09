[![Build Status](https://travis-ci.org/adolfopa/racket-mustache.svg)](https://travis-ci.org/adolfopa/racket-mustache)
[![Coverage Status](https://coveralls.io/repos/adolfopa/racket-mustache/badge.svg)](https://coveralls.io/r/adolfopa/racket-mustache)
[![License](https://img.shields.io/badge/license-LGPL2-blue.svg)](http://opensource.org/licenses/LGPL-2.1)

# Mustache for Racket

A Racket implementation of [Mustache](http://mustache.github.io).

## Installation

```sh
$ raco pkg install "git://github.com/adolfopa/racket-mustache.git?path=mustache"
```

## Quick Introduction

Lets create a simple Mustache template that outputs a single value.  First, create
a file named `simple.ms` (the `.ms` extension is not required; you can use whatever
you want).

```mustache
#lang mustache
Hey! this is my {{adjective}} Mustache template.
```

Note that templates *must* have `#lang mustache` as the first thing in the file for
Racket to detect it as a Mustache template.

Templates are compiled into regular racket modules.  To invoke the template from another
module, simply require it and call the `render` function.  Type this into a `simple-test.rkt`
file in the same directory where `simple.ms` is:

```racket
#lang racket

(require "simple.ms") ; imports the `render` function

(module+ main
  (render (hash "adjective" "first") (current-output-port)))
```

The render function has the following signature:
```racket
(: render ((Dict String Any) Port -> Void))
```
where the first argument is a dict mapping names (strings) to values (any racket value);
the second argument is the port where the template will write its output.

If you execute the `simple-test.rkt` script you should see something like:
```sh
$ racket simple-test.rkt
Hey! this is my first Mustache template.
```

And that's it!
