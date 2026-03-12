# Contributing
Ah, you want to contribute? Welcome welcome, new people are always welcome. Keep in mind this is a volunteer project, and the developer may take some time to respond.

# Architecture
The code lives in `/src/`. You might see `all.gs`, and might be tempted to modify that. Don't. `all.gs` is the place where *stable releases* live. When a stable release is made, it's packaged into `all.gs` so it's easy for people to download it. The actual code that you should modify lives in `/src/`. 

`clojette.gs` is the entrypoint to the project. It imports everything, and has the main functionality.

`clojette-core.gs` is the core of the interpreter. It has the evaluator, lexer, and tokenizer. All special forms are in eval().

`clojette-env.gs` has the Clojette environment setup. It handles argument binding and the environment stack frames.

`clojette-stdlib.gs` houses the parts of the Clojette standard library that interface with GreyScript. It could be made smaller, if there is a need to embed Clojette somewhere.

`clojette-interop.gs` populates the global environment with the native variables and functions.

`macros.clj` has the standard macros. Enough said.

`stdlib.clj` has the Clojette standard library that is self hosted. Prefer this over the GreyScript standard library.

`tests.clj` houses our test suite. These must be tested against a stable release if they are to be changed.

# Style
Clojette has style guidlines that should be followed at all times. Following these style guidlines helps when others read your code.

## GreyScript
Clojette follows the GreyScript-specific conventions. GreyScript functions and variables should use camelCase. Any time you handle a function, you MUST dereference it with `@`. That will call the function, which you might not want.

Additionally, some objects have a classID tagging convention. Currently we have four of these; `fn`, `recur`, `native`, and `error`. New runtime objects should follow the same pattern. New objects should also use the `@__runtimeTag__` sentinel. The sentinel is used, so that user input cannot falsely masquerade as runtime objects.

### what to avoid
Do not store funcRef objects without using a `@`. This calls the function. We do not want that.

Do not commit print() debug calls. These are useful when debugging, but we need to keep the code clean.

Do not use `isa map` check without also checking `hasIndex("__tag__")` when distinguishing between runtime objects from plain maps.


## Clojette
Clojette functions and variables should use kebab-case. Predicates (things returning a boolean) should end in `?`, whereas mutating forms (things that change state outside of itself) should end in `!`. In short, Clojette follows the Clojure conventions.

# Tests
Always test your changes against our test suite to check for regressions. In case you are contributing new tests, make sure said tests are tested against a stable release of Clojette. Code changes and test changes should never be pulled in at the same time. If your pull request does not pass all the tests, it will not be accepted.

# Thanks for contributing!
Contributing is always encouraged, and warm thanks to anyone who contributes!
