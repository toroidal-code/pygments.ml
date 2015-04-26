# pygments.ml

An OCaml wrapper for the Python [pygments syntax highlighter](http://pygments.org/), forked from
[pygments.rb](https://github.com/tmm1/pygments.rb)

pygments.ml works by talking over a simple pipe to a long-lived 
Python child process.

Each OCaml process that runs has its own 'personal Python'.
If a Python process dies, a new one will be spawned on the next 
pygments.ml request.

## system requirements

- Python 2.5, Python 2.6, or Python 2.7. You can always use Python 2.x from a `virtualenv` if
  your default Python install is 3.x.

- pygments installed to your site-packages.

- simplejson installed to your site-packages.


Note: The site-packages location in pygments.ml is set to`/usr/lib/python2.7/site-packages/pygments/` by default, but can be changed by the `PYGMENTS_PATH` environment variable.

## usage

``` ocaml
Pygments.highlight ~opts:[("lexer", `String ocaml)] "let x = 5;;"
```

Encoding and other lexer/formatter options can be passed in via an
options object:

``` ocaml
Pygments.highlight ~opts:[("options", `Assoc [("encoding", `String "utf-8")])] "code"
```

pygments.ml defaults to using an HTML formatter. 
To use a formatter other than `html`, specify it explicitly
like so:

``` ocaml
Pygments.highlight ~opts:[("formatter", `String "bbcode")] "code"
Pygments.highlight ~opts:[("formatter", `String "terminal")] "code"
```

<!-- To generate CSS for HTML formatted code, use the `#css` method: -->

<!-- ``` ruby -->
<!-- Pygments.css -->
<!-- Pygments.css('.highlight') -->
<!-- ``` -->

<!-- To use a specific pygments style, pass the `:style` option to the `#css` method: -->

<!-- ``` ruby -->
<!-- Pygments.css(:style => "monokai") -->
<!-- ``` -->


<!-- Other Pygments high-level API methods are also available. -->
<!-- These methods return arrays detailing all the available lexers, formatters,  -->
<!-- and styles. -->

<!-- ``` ruby -->
<!-- Pygments.lexers -->
<!-- Pygments.formatters -->
<!-- Pygments.styles -->
<!-- ``` -->

To use a custom pygments installation, specify the path to
`Pygments.start`:

``` ruby
Pygments.start ~pygments_path:"/path/to/pygments"
```

Logging is performed using syslog.

By default pygments.ml will timeout calls to pygments that take over 8 seconds. You can change this
by setting the environmental variable `MENTOS_TIMEOUT` to a different positive integer value.

## license

The MIT License (MIT)

Copyright (c) Ted Nyman and Aman Gupta, 2012-2013  
Copyright (c) Katherine Whitlock, 2015

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and 
associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, 
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
