Classic Problem Solver
======================

API
---

``depth-first-solver`` creates depth first solver.

.. code:: hy

   (depth-first-solver :is-goal goal?
                       :operators operators
                       :is-identical identical?)

   
``breadth-first-solver`` creates breadth first solver.

.. code:: hy

   (breadth-first-solver :is-goal goal?
                         :operators operators
                         :is-identical identical?)

                         
``best-first-solver`` creates best first solver.

.. code:: hy

   (best-first-solver :is-goal goal?
                      :operators operators
                      :is-identical identical?
                      :distance distance-left)

``goal?`` is a function that accepts a single parameter ``state`` and
returns ``true`` or ``false`` indicating if the goal has been reached.

``operators`` is a function that accepts a single parameter and returns
a list of tuples where first element is a function that can create a new
state from old one and second element is textual description of the transition.

``identical?`` is a function accepting two states and returning ``true`` or
``false`` indicating if the states are considered equal. This function is
used to detect loops in search path.

``distance`` is a function that estimates distance left for given state. It
is used to optimize search path in best first search.


Example
-------

Classic problem solver (CPS for short) is a solver based on simple search and
goal detecting routine. Systems starts from an initial state and expands
through problem space by trying different operations according to given
search criteria.

If we wanted to solve the following problem:

    You have 2 jugs, with capacity of 4 and 5 liters. Your task is to measure
    2 liters of water. You have unlimited amount of water in your disposal.

A high level example of typical solver initialization and execution:

.. code:: hy

   (require stilpo.cps)
   (import [stilpo.cps [breadth-first-solver valid-operators]])
          
   (def b-solve (breadth-first-solver :is-goal goal?
                                      :operators operators
                                      :is-identical identical?))
   (-> (b-solve initial-state)
       (print-solution))

``breadth-first-solver`` is a function that will create a solver that uses
breadth first search when called. This solver can then be used to solve one
or more problems (it doesn't retain state between calls).

We represent state of our problem as a dictionary (any other data structure
would work too, stilpo isn't that particular about it). At the beginning,
there are two jugs, both empty:

.. code:: hy

   (def initial-state {:jug-4 0
                       :jug-5 0})

Detecting goal in our case is simple. When ever one of the jugs holds exactly
two liters of water, we're done:

.. code:: hy

   (defn goal? [state]
     (or (= (:jug-4 state) 2)
         (= (:jug-5 state) 2)))
                       
CPS needs to know which operators it can perform to any given state. Operator
is just a function that when applied to a state, will return a new state. You
are free to structure your code in the way you prefer, but stilpo has an
utility functions for building operators and detecting when they can be
applied.

``operator`` macro is used to define special function that represents an
operation that can be done to a ``state``:

.. code:: hy

   (operator empty-jug-4 "pour 4 liter jug empty"
             (> (:jug-4 state) 0)
             {:jug-4 0
              :jug-5 (:jug-5 state)})

First parameter is name of the function being defined, second one is
textual description that can be printed out to specify solution to the
problem. Third parameter is a form that returns ``true`` if operator is legal
for given state. Rest of the code is used to create a new state that has
been modified (4 liter jug poured empty in this example).

Each discrete action is defined as an operator like above and then packed
into a function that can check which operators are valid for given state and
return their application:
                       
.. code:: hy

   (defn operators [state]
     "all valid operators for given state and their descriptions"
     (valid-operators state empty-jug-4 empty-jug-5
                      fill-jug-4 fill-jug-5
                      pour-4-to-5 pour-5-to-4))


Final tool we need to define is detection of identical states. This is used
by search algorithm to prune possible loops from the solution:

.. code:: hy

   (defn identical? [state1 state2]
     (and (= (:jug-4 state1) (:jug-4 state2))
          (= (:jug-5 state1) (:jug-5 state2))))

We of course would like to print out our solution, so we define
``pretty-print`` to do that task for us:

.. code:: hy

   (require hy.contrib.anaphoric)
          
   (defn pretty-print [path]
     (when path
       (ap-each path
                (cond [(in :action it)
                       (print (.format "{0} (jugs: {1} and {2})"
                                       (:desc (:action it))
                                       (:jug-4 (:state it))
                                       (:jug-5 (:state it))))]
                      [true (print "starting")]))))

Function simple walks the path and prints out textual info of action taken and
amount of water held by each jug:

   | starting
   | fill 4 liter jug with water (jugs: 4 and 0)
   | pour water from 4 liter jug to 5 liter jug (jugs: 0 and 4)
   | fill 4 liter jug with water (jugs: 4 and 4)
   | pour water from 4 liter jug to 5 liter jug (jugs: 3 and 5)
   | pour 5 liter jug empty (jugs: 3 and 0)
   | pour water from 4 liter jug to 5 liter jug (jugs: 0 and 3)
   | fill 4 liter jug with water (jugs: 4 and 3)
   | pour water from 4 liter jug to 5 liter jug (jugs: 2 and 5)
