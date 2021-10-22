# URI Parse

A library for fast, rigorous URI syntax validation and parsing.

Note that this is alpha quality software, and its interface is still being
developed.

## Installation

You'll need to install [Trivial US-ASCII](https://git.sr.ht/~pyramidion/trivial-us-ascii) and [ABNF Match](https://git.sr.ht/~pyramidion/trivial-us-ascii) first.

URI Parse requires [ASDF](https://common-lisp.net/project/asdf/), the
Common Lisp world's de facto standard build facility. Most Common Lisp
implementations ship with ASDF, so chances are you don't need to install it
yourself.

You'll need to [configure ASDF to find URI Parse](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html).

If you're in a hurry, and run a *nix system, just do this:

```bash
$ mkdir -p ~/.local/share/common-lisp/source

$ git clone https://git.sr.ht/~pyramidion/uri-parse \
  ~/.local/share/common-lisp/source/
```

ASDF should find the package there and make it available to your Common Lisp
implementation. Subsequently, you will be able to `require` the package in
your REPL, and include it as a dependency to your own projects using ASDF.

At some point, I'll see about including it in [Quicklisp](https://www.quicklisp.org/beta/).

## Usage

Right now there's just a single low-level function: `parse`, which takes a
`(simple-array (unsigned-byte 8) (*))` containing a URI. It returns seven
components: `scheme`, `userinfo`, `host`, `port`, `path`, `query`, and
`fragment`.

```lisp
CL-USER> (require :uri-parse)
NIL

CL-USER> (defparameter *uri* "http://www.ics.uci.edu/pub/ietf/uri/#Related")
*URI*

CL-USER> (defparameter *uri-octets*
                       (trivial-us-ascii:ascii-string-code
                         '(simple-array (unsigned-byte 8) (*))
                         *uri*))
*URI-OCTETS*

CL-USER> (uri-parse:parse *uri-octets* 0 (length *uri-octets*))
"http"
NIL
"www.ics.uci.edu"
NIL
"/pub/ietf/uri/"
NIL
"#Related"
```

## Links

* [Repository](https://sr.ht/~pyramidion/uri-parse/)

## Patches

Patches are welcome.

## License

URI Parse is licensed under the GNU Affero General Public License v3.

See LICENSE and AGPLv3.
