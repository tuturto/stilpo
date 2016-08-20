;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2016 Tuukka Turto
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; how to measure 2 liters of water if you have 5 and 4 liter jugs at your disposal?

(require hy.contrib.anaphoric)
(require stilpo.cps)

(import [stilpo.cps [breadth-first-solver depth-first-solver
                     valid-operators]])

(defn pretty-print [path]
  (when path
    (ap-each path
             (cond [(in :action it) (print (.format "{0} (jugs: {1} and {2})"
                                                    (:desc (:action it))
                                                    (:jug-4 (:state it))
                                                    (:jug-5 (:state it))))]
                   [true (print "starting")]))))

(def state {:jug-4 0
            :jug-5 0})

(defn goal? [state]
  (or (= (:jug-4 state) 2)
      (= (:jug-5 state) 2)))

(defn operators [state]
  "all valid operators for given state and their descriptions"
  (valid-operators state empty-jug-4 empty-jug-5
                   fill-jug-4 fill-jug-5
                   pour-4-to-5 pour-5-to-4))

(operator empty-jug-4 "pour 4 liter jug empty"
          (> (:jug-4 state) 0)
          {:jug-4 0
           :jug-5 (:jug-5 state)})

(operator empty-jug-5 "pour 5 liter jug empty"
          (> (:jug-5 state) 0)
          {:jug-5 0
           :jug-4 (:jug-4 state)})

(operator fill-jug-4 "fill 4 liter jug with water"
          (< (:jug-4 state) 4)
          {:jug-4 4
           :jug-5 (:jug-5 state)})

(operator fill-jug-5 "fill 5 liter jug with water"
          (< (:jug-5 state) 5)
          {:jug-5 5
           :jug-4 (:jug-4 state)})

(operator pour-4-to-5 "pour water from 4 liter jug to 5 liter jug"
          (and (> (:jug-4 state) 0)
               (< (:jug-5 state) 5))
          (setv jug-5 (:jug-5 state))
          (setv jug-4 (:jug-4 state))
          (setv capacity-left (- 5 jug-5))
          (setv poured (if (> capacity-left jug-4)
                         jug-4
                         capacity-left))
          (setv jug-4 (- jug-4 poured))
          (setv jug-5 (+ jug-5 poured))
          {:jug-4 jug-4
           :jug-5 jug-5})

(operator pour-5-to-4 "pour water from 5 liter jug to 4 liter jug"
          (and (> (:jug-4 state) 0)
               (< (:jug-5 state) 5))
          (setv jug-5 (:jug-5 state))
          (setv jug-4 (:jug-4 state))
          (setv capacity-left (- 4 jug-4))
          (setv poured (if (> capacity-left jug-5)
                         jug-5
                         capacity-left))
          (setv jug-4 (+ jug-4 poured))
          (setv jug-5 (- jug-5 poured))
          {:jug-4 jug-4
           :jug-5 jug-5})

(defn identical? [state1 state2]
  (and (= (:jug-4 state1) (:jug-4 state2))
       (= (:jug-5 state1) (:jug-5 state2))))

(def b-solve (breadth-first-solver :is-goal goal?
                                   :operators operators
                                   :is-identical identical?))

(def d-solve (depth-first-solver :is-goal goal?
                                 :operators operators
                                 :is-identical identical?))

(print)
(print "solving jugs problem with breadth first search")
(-> (b-solve state)
    (pretty-print))

(print)
(print "solving jugs problem with depth first search")
(-> (d-solve state)
    (pretty-print))
