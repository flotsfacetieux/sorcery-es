Sorcery-es
=========

A old school 2D Game written in Common Lisp with Lispbuilder-SDL with the architectural pattern [Entity Component System][1].

How do I play it?
-----------------

Only tested under Linux.

You'll need to install :
- [Steel Bank Common Lisp][2]
- [the Quicklisp package tool][3]
- [cl-entity-system][4]

Clone the repository to your local-projects quicklisp directory.
```
cd ~/quicklisp/local-projects
git clone https://github.com/flotfacetieux/sorcery-es
```

Start SBCL and type the following at the REPL prompt:
```lisp
(ql:quickload :sorcery-es)
(se:start)
```

The game is keyboard controlled :
- 'q' : quit game
- key up : up
- key left : left
- key right : right
- space : fire

What does it look like?
-----------------------

[Youtube video][5]

[1]: https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system
[2]: http://www.sbcl.org/
[3]: http://www.quicklisp.org/
[4]: https://github.com/flotfacetieux/cl-entity-system
[5]: https://www.youtube.com/channel/UCEozyq6XnDKQvRLYimYp1uA