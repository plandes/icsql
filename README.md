# Interface to ciSQL for interacting with relational databases.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

This library provides an [Emacs SQL mode] integration to the [ciSQL] program.
In addition it provides support for multiple buffers and fast/easy buffer (for
each SQL connection) using the [buffer manage] library.

Features:

* [Emacs SQL mode] integration.
* Utilization of JVM based [ciSQL]
* Create customize connections using the Emacs customization system or start an
  instance with no connection and then create the connection once you're in
  [ciSQL].
* Provides support for multiple buffers and fast/easy buffer (for each SQL
  connection) using the [buffer manage] library.


## Obtaining

I have not put a few last touches on this before publishing
to [Melpa](https://melpa.org/).  Create
an [issue](https://github.com/plandes/icsql/issues/new) and I'll prioritize it
for anyone who wants it.


## Usage

1. Copy [icsql.el](icsql.el) to your `~/.emacs.d` directory (or whever you keep
   ad-hoc libraries).  See the [note](#obtaining) regarding making this a Melpa
   library.
2. In your `~/.emacs` file add: `(require 'icsql)`
3. Optionally customize SQL connection(s) with `M-x customize-variable RET
   icsql-connections`.  See the [connection](#connections) section for
   specifics on each field.
4. In Emacs: `M-x icsql` and `RET` to configure the library in `ciSQL` or give
   the customized connection provided in the previous step.


## Connections

The fields for connections with `icsql-connections` should be left blank or the
default value if there is no meaningful value.  The fields are:

* **Name**: Whatever string is meaningful to you.
* **Product**: The product of the backing JDBC connection (i.e. `mysql`,
  `oracle` etc) or leave the default for custom JDBC drivers that have no
  correlated product.  See [below](#product) for more information on product.
* **Host**: the fully qualified host name to connect to (if any)
* **Database**: The data base name, or in some cases, the file (*sqlite*
  driver) or directory (*csv* driver).
* **User**: The login user name if any.
* **Password**: The login password if any.
* **Configuration**: A list of key value pairs that set variable(s) on [ciSQL] startup.

See the documentation in [connecting to a database] to specifics on each field.

### Product

For more information on Emacs SQL products see the `sql-product-alist`
variable.  You can also get a full list of products with `M-:` and the following Lisp:
```emacs-lisp
(mapcar 'car sql-product-alist)
```


## Command Line Help

You can get command line help to the subordinate command line program [ciSQL]
using the following `icsql-help-command-line`.


## License

Copyright © 2018-2019 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->

[melpa-link]: https://melpa.org/#/icsql
[melpa-stable-link]: https://stable.melpa.org/#/icsql
[melpa-badge]: https://melpa.org/packages/icsql-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/icsql-badge.svg
[travis-link]: https://travis-ci.org/plandes/icsql
[travis-badge]: https://travis-ci.org/plandes/icsql.svg?branch=master

[Emacs SQL mode]: https://www.emacswiki.org/emacs/SqlMode
[ciSQL]: https://github.com/plandes/cisql
[buffer manage]: https://github.com/plandes/buffer-manage
[connecting to a database]: https://github.com/plandes/cisql#connecting-to-a-database
