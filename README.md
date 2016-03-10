# cl-dom-templating

This project is based on the ideas presented [here](http://camendesign.com/code/dom_templating)

I am familiar with that project (a fork of it really) and though it would be a challenging enough task to port the library to CL for fun, and to learn CL from actual experience.

## Setting up

This project makes use of 2 libraries:

* [cxml](https://common-lisp.net/project/cxml/)
* [plexippus-xpath](https://common-lisp.net/project/plexippus-xpath/)

cxml is already a package you can load with quicklisp or ASDF.

You will need to manually set up plexippus-xpath for yourself though, by registering the projects .asd file. Please also read the project's documentation to learn more about it.
