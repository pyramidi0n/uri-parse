# URI Parse

A fast, RFC-compliant URI validator and parser.

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Performance](#performance)
5. [Links](#links)
6. [Patches](#patches)
7. [License](#license)

## Overview

This library complies with [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) and
includes a comprehensive test suite. It provides validation and parsing that's
much more robust than most regex-based solutions.

And it's fast.

## Installation

URI Parse is available on [Ultralisp](https://ultralisp.org/) and is easy to
install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install URI Parse:

```lisp
CL-USER> (ql:quickload :uri-parse)
```

## Usage

You can parse URI strings:

```lisp
CL-USER> (require :uri-parse)
NIL

CL-USER> (uri-parse:parse "http://www.ics.uci.edu/pub/ietf/uri/#Related")
"http"
NIL
"www.ics.uci.edu"
NIL
"/pub/ietf/uri/"
NIL
"Related"

CL-USER> (uri-parse:parse "http://www.ics.uci.edu/pub/ietf/uri/#Related"
                          :plist t)
(:SCHEME "http" :USERINFO NIL :HOST "www.ics.uci.edu" :PORT NIL :PATH
 "/pub/ietf/uri/" :QUERY NIL :FRAGMENT "Related")
```

Or octet sequences:

```lisp
CL-USER> (require :uri-parse)
NIL

CL-USER> (uri-parse:parse-octets
          (trivial-us-ascii:ascii-string-code
           '(simple-array (unsigned-byte 8) (*))
           "http://www.ics.uci.edu/pub/ietf/uri/#Related")
          0
          (length "http://www.ics.uci.edu/pub/ietf/uri/#Related"))
"http"
NIL
"www.ics.uci.edu"
NIL
"/pub/ietf/uri/"
NIL
"Related"

CL-USER> (uri-parse:parse-octets
          (trivial-us-ascii:ascii-string-code
           '(simple-array (unsigned-byte 8) (*))
           "http://www.ics.uci.edu/pub/ietf/uri/#Related")
          0
          (length "http://www.ics.uci.edu/pub/ietf/uri/#Related")
          :plist t)
(:SCHEME "http" :USERINFO NIL :HOST "www.ics.uci.edu" :PORT NIL :PATH
 "/pub/ietf/uri/" :QUERY NIL :FRAGMENT "Related")
```

## Performance

Parsing is fairly quick, and runs in linear time, though the parser does cons:

```lisp
CL-USER> (time (dotimes (c 100000)
                 (uri-parse:parse "http://www.ics.uci.edu/pub/ietf/uri/#Related")))
Evaluation took:
  0.336 seconds of real time
  0.338207 seconds of total run time (0.338165 user, 0.000042 system)
  100.60% CPU
  1,149,893,872 processor cycles
  246,412,608 bytes consed

NIL

CL-USER> (time (dotimes (c 100000)
                 (uri-parse:parse-octets
                  (trivial-us-ascii:ascii-string-code
                   '(simple-array (unsigned-byte 8) (*))
                   "http://www.ics.uci.edu/pub/ietf/uri/#Related")
                  0
                  (length "http://www.ics.uci.edu/pub/ietf/uri/#Related"))))
Evaluation took:
  0.344 seconds of real time
  0.342664 seconds of total run time (0.334695 user, 0.007969 system)
  99.71% CPU
  1,165,090,342 processor cycles
  246,412,592 bytes consed

NIL
```

## Links

* [Repository](https://sr.ht/~pyramidion/uri-parse/)

## Patches

Patches are welcome.

## License

URI Parse is licensed under the two-clause BSD license.

See LICENSE.
