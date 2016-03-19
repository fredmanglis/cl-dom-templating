# cl-dom-templating

This project is based on the ideas presented [here](http://camendesign.com/code/dom_templating)

I am familiar with that project (a fork of it really) and though it would be a challenging enough task to port the library to CL for fun, and to learn CL from actual experience.

## Setting up

This project makes use of 2 libraries:

* [cxml](https://common-lisp.net/project/cxml/)
* [plexippus-xpath](https://common-lisp.net/project/plexippus-xpath/)

cxml is already a package you can load with quicklisp or ASDF.

You will need to manually set up plexippus-xpath for yourself though, by registering the projects .asd file. Please also read the project's documentation to learn more about it.

## API

### Instantiation

You can instantiate by doing:

	(defparameter *template* (make-instance 'domtemplate :file-path "test.html"))

where `:file-path` is a string representing the path to the file (Maybe I should use a pathname object?)

I have provided a file called "test.html" which you can use to follow along with the examples here, but if you want, you can create your own and use it instead.

### Output

To get the HTML, do:

	(html *template*)

### set-value

To set the value of an item in the DOM, you do:

	(set-value *template* :xpath "//p" :value "Changed the paragraph text")

or say, to change an attribute:

	(set-value *template* :xpath "//p/a/@href" :value "elsewhere.html")

### query

To return a list of elements that match the xpath expression:

	(query *template* "//p")

this return all &lt;p&gt; elements in the file

### remove

Not implemented

### add-class

Not implemented

### set

Not implemented

### repeat

Not implemented
