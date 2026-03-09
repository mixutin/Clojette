# Clojette
Clojette is an opinionated and feature-full Clojure-like Lisp for GreyHack written in GreyScript/MiniScript. Clojette supports runtime-expanded macros, quasiquoting, splice unquoting, threading macros, and MiniScript interop. Many of the language features are implemented as they are in Clojure, so anyone familiar with Clojure will feel at home with Clojette. Clojette has keywords, let bindings, string literals, and other such features.

# Why?
I made Clojette in around 4 days. As you all know, GreyHack uses MiniScript as its scripting language. I absolutely loathe the language. What does a programmer do in that situation? He of course, makes his own... Clojette is a lisp for the simple reason that I started learning Lisp around a week ago and got the inspiration to write my own Lisp. I have been meaning to write my own language for the game for as long as I have known about the horrors of GreyScript. And so naturally, I wrote a Lisp for GreyScript. Clojette was written in around ~30 or so hours of active development time. The interpreter core was done on day 1 in around 3 hours, and additions such as macros were added on day 2. The current environment model is modelled after the Structure and Interpretation of Computer Programs (SICP) Lisp environment model. To know more, go read the book.

# Running the language
To run the language, cope `all.gs` into your game environment. Then build `all.gs`, and you'll have a Clojette runtime. To modify what files Clojette runs, you can easily modify it to load any file you want. Alternatively, you can use the REPL to import files using the `import` special form like this; `(import "/path/to/file.clj")`. In the future (possibly as early as 1.0.1) I will add better runtime usage. Currently the language includes the runtime and the standard library, along with the test suite.

# Syntax
Here's a bit of info on the syntax of Clojette. For more info, visit `DOCS.md`. Alternatively, go read the [Clojure documentation](https://www.clojure.org/guides/learn/clojure).

## Atoms
Atoms are a very basic data structure in any Lisp. Atoms cover numerals, strings, booleans, and keywords. Keywords are special in the sense that they are self-evaluating.

## Lists and Vectors.
Everything in Clojette is made of lists. Lists are the basic data structure you will work with all the time in Clojette. A list can be instantiated with the `(...)` syntax. The first element of a list is always the callable. Callable is a function, or anything else that can be called. `(+ 1 2 3)` is a list, where + is the function to be called, and 1, 2, and 3 are the arguments to the function. Simple enough.

The other very special data structure is Vectors. Vectors are instantiated with the `[...]` syntax. Vectors are a data structure, and are used for example when you define functions.

## Defining functions
`defn` defines a function. It is a macro that combines `def` and `fn`. `fn` defines an anonymous function.

## let
`let` is a special form that lets you bind vectors as such: `(let [x 1 y 2] ...)`. Big deal.

## do
`do` is a special form for sequencing things. You can use `do` as such: ``(do (x))`.

## if
`if` Works the exact same way as in Clojure.
`if` is the most important conditional expression - it consists of a condition, a "then", and an "else". `if` will only evaluate the branch selected by the conditional.
```clojure
=> (str "2 is " (if (even? 2) "even" "odd"))
2 is even
```

## recur
`recur` is used for recursion. Since GreyScript doesn't support TCO out-of-the-box, Clojette uses the same thing as Clojure does; it has a `recur` keyword for recursion! Under the hood it uses looping, but it might as well be recursion for your needs.

## set!
`set!` can be used to mutate existing bindings. Very dangerous.

## try/catch
Clojette, unlike GreyScript, has `try/catch`. This is very useful for actually writing programs. Errors are propagated through the program, all the way up to the original callsite. Pretty cool, huh?

## GS Interop
Clojette, much like Clojure, has interop with its host language. The syntax is very much similar to Clojure's interop syntax. In short Clojette uses, `.function obj args` for its interop.

A simple example of Clojette interop:
```clojure
(-> (get_shell)
  (.host_computer)
  (.File "/etc/passwd")
  (.get_content)
  (println))
```

## Macros
To learn about Macros, go to `DOCS.md`

## Comments
Clojette uses `;` for comments. As a convention, use `;;` for comments.

## Conventions
Speaking of conventions, there are a few conventions that Clojette has. Anything returning a boolean value should have a `?` at the end. Anything with side-effects should have a `!` at the end.

For the rest, go to the actual docs, please.

# Notable differences to Clojure
Clojette isn't a 1:1 port of Clojure. Clojette is missing a few things, such as `#{}`, no `@` dereferencing yet, and no `#()` reader macro for anonymous functions, nor does it have namespaces. These might or might not be added in future updates.

# TODO
There are currently a few features that are needed.

1. Runtime gensym. Macros need this.
2. Runtime macroexpand.
3. Persistent vectors. I am quite sure that other peristent data structures could be useful too.
4. Stack traces to try/catch. This is simple to add.
5. `#{}`, `@` deref, and `#()` reader macros.
6. Expose `eval()` to the user.

# Contributing
Pull requests are welcome. To get to know the ins and outs of contributing, check out `CONTRIBUTING.md`. Also make sure to run the tests before making a pull requests. All tests should pass, and if they do not pass, any PR will not be accepted. To make sure regressions do not happen, the test suite MUST pass before a PR is accepted.

# Acknowledgements
Although I did not know about [Glosure](https://github.com/mahocitrus/Glosure) when I started the project, it is the first Lisp that was implemented in GreyScript. Big props to mahocitrus.

# Resources
Here are a few resources for learning Lisp. Clojette intentionally feels a lot like Clojure, so I am also linking Clojure resources here.


https://www.clojure.org/guides/getting_started
https://www.clojure.org/guides/learn/clojure

https://hypirion.com/musings/understanding-persistent-vector-pt-1
https://gigamonkeys.com/book/syntax-and-semantics.html

## Training problems
https://projecteuler.net/archives;page=1

## books
https://www.braveclojure.com/foreword/
https://annas-archive.gd/md5/ca4e2ea298f40bffd95757bda1eed297
https://gigamonkeys.com/book/

## Wikipedia articles...
https://en.wikipedia.org/wiki/Purely_functional_programming
https://en.wikipedia.org/wiki/Functional_programming
https://en.wikipedia.org/wiki/Lisp_(programming_language)
https://en.wikipedia.org/wiki/Clojure

## Implementing lisp:
https://norvig.com/lispy.html
https://norvig.com/jscheme.html
https://github.com/nukata/lisp-in-typescript
https://maryrosecook.com/blog/post/little-lisp-interpreter
https://github.com/Indy9000/lisper
[(in Japanese)](https://web.archive.org/web/20101007171742/http://www.aoky.net/articles/peter_norvig/lispy.htm)
https://zenn.dev/ytaki0801/articles/042cfa374223b3a5c03c
https://qiita.com/41semicolon/items/d59f00ebb70b14fdb4e3

