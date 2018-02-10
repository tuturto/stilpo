Tiny Rule Engine
================

API
---

``create-tre`` creates a tiny rule engine

.. code:: hy

   (setv tre (create-tre "Example" :debug False))

``assert!`` asserts a fact into a given rule engine

.. code:: hy

   (assert! tre '(hy is lisp))

``rule`` creates a new rule and inserts it into a rule engine

.. code:: hy

   (rule tre '(?x is lisp)
         (assert! '(?x is awesome)))

``run`` execute tiny rule engine until all rules and assertions are processed

.. code:: hy

   (run tre)

``show`` show assertions associated with given symbol

.. code:: hy

   (show 'hy)

``true?`` check if given assertion holds

.. code:: hy

   (true? '(hy is awesome))

Example
-------

Tiny rule engine is pattern directed inference system that operates on symbols
and patterns. Essentially, it deduces new assertions based on existing
assertions and rules.

For example, we can deduct family relations:

First step is to initialize tiny rule engine and bind a symbol to it:

.. code:: hy

   (setv tre (create-tre "family"))

Assertions (true statements) are created with ``assert!``. Once a truth has been
asserted, there is no way to remove it. This is because doing so would have to
remove all rules and assertions that it might have created and their results and
so on. Keeping track of web of assertions and rules would have been rather
complicated and error prone system, so it was left out.

.. code:: hy

   (assert! tre '(Alice is parent of Bob))
   (assert! tre '(Bob is parent of Charlie))

Rules are used to create new assertion and rules based on existing ones. They
consist of a pattern and body. When tiny rule engine executes a rule, it
processed through all assertions, checking if any of them match the pattern.
When a match is found, body of the rule is executed. Special notation is used
to introduce free variables in the pattern that can then be used in the
body:

.. code:: hy

   (rule tre '(?x is parent of ?y)
         (assert! tre '(?y is children of ?x)))

   (rule tre '(?x is parent of ?y)
         (rule tre '(?y is parent of ?z)
               (assert! tre '(?x is grand-parent of ?z))))

   (rule tre '(?x is grand-parent of ?y)
         (assert! tre '(?y is grand-children of ?x)))

Final step in our example is to execute the engine and review the results,
which should show that Alice indeed is grand parent of Charlie:

.. code:: hy

   => (run tre)
   => (show tre 'Alice)
   '(Alice is parent of Bob)
   '(Alice is grand-parent of Charlie)
   '(Charlie is grand-children of Alice)
   '(Bob is children of Alice)

   => (true? tre '(Alice is grand-parent of Charlie))
   True

The order of adding rules and assertions into tiny rule engine doesn't matter.
Engine will keep processing rules until no further changes occur in assertions.
It is even possible to run tiny rule engine in REPL, working with rules and
assertions step by step.
