#Dūce
- Old English for diver/duck.
- A basic Ocaml coded static analyzer for C coders. It is meant to be used as a
  companion in the debugging process, not as a debugger itself.


##A companion?
In the rubber duck debugging method, you need something or someone to
explain your code to. During the process, you might think:
"Hey! Maybe it comes from here". When doing it with a human being understanding
what you say, this person might say the same at some moment.

This is exactly what this dūce is supposed to do. Warn you that maybe you are
doing something wrong at some places and point it to you.


##How do I use it?
As it is coded in OCaml, you can use the 'ocaml' interpreter.

If you want to compile it, you will need 'ocamlopt' and 'make'.
- `make` will get you the **duce** binary file
- `make check` will make and run tests


##Goal
I am doing this project in order to improve my OCaml skills, learn more about C
and techniques used to identify potential runtime errors at an early stage.


##Grammar
The C99 grammar is [here](http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1124.pdf)


##How do I contribute?
As the goal for me is to learn things, I would prefer to do the entire project
by myself. However, having someone participating can be instructive, this is
why some oher people could sometime join in.


##Authors
- fantazio

[fantazio on wordpress](https://corentindsz.wordpress.com)

[fantazio on tumblr](corentindsz.tumblr.com)

[fantazio on twitter](https://twitter.com/CorentinDsz)
