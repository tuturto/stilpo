Tools
=====

Tools module contain assortment of tools that are generally useful in writing
problem solvers.

unify
-----

``unify`` tries to unify an assertion with a pattern, while capturing joker and
wildcard values. If unification is successful, a dictionary containing captured
variables is returned. If unification failed, ``None`` is returned instead.

Interface ``(unify [assertion pattern bindings unique-symbols])``

   * ``assertion`` s-expression

   * ``pattern`` pattern as s-expression, may contain ``?x`` and ``*x``

   * ``bindings`` dictionary containing predefined values for variables

   * ``unique-symbols`` tuple of tuples defining variables that should be unique

.. code:: hy

   => (unify '(Hello World) '(Hi there, ?x) {} (,))
   None

   => (unify '(Hello World) '(Hello ?x) {} (,))
   {'?x 'World}

   => (unify '(Hello Charlie Brown) '(Hello *x) {} (,))
   {'*x '(Charlie Brown)}

   => (unify '(foo bar baz) '(?x bar ?y) {} (,))
   {'?x 'foo '?y baz}

   => (unify '(foo bar foo) '(?x bar ?y) {} (,))
   {'?x 'foo '?y foo}

   => (unify '(foo bar foo) '(?x bar ?y) {} (, (, '?x '?y)))
   None

fill-assertion
--------------

``fill-assertion`` replaces variables in assertion with their respective values
from supplied bindings.

Interface ``(fill-assertion [assertion bindings])``

   * ``assertion`` s-expression containing ?x and *x variables

   * ``bindings`` dictionary containing variables and their values

.. code:: hy

   => (fill-assertion '(Hello ?x) {'?x 'World})
   '(Hello World)
