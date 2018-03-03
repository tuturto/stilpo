;; -*- coding: utf-8 -*-
;;
;; Copyright (c) 2018 Tuukka Turto
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

;; solve 8 queens problem with classical problem solver

(import [stilpo.cps [depth-first-solver]])

(defn conflicts? [queens]
  "can any of the queens take any other queen?"
  (any (map (fn [x] (can-take-any? x queens)) queens)))

(defn can-take-any? [queen queens]
  "can queen take any other queen?"
  (any (map (fn [x]
              (and (not (= x queen))
                   (or (= (first x) (first queen))
                       (= (second x) (second queen))
                       (= (abs (- (first x) (first queen)))
                          (abs (- (second x) (second queen)))))))
            queens)))

(defn identical? [queens-a queens-b]
  "are two states identical?"
  (= queens-a queens-b))

(defn display [queens]
  "display board"
  (if (:path queens)
    (do (setv board (:state (last (:path queens))))
        (defn separator []
          "print separator line"
          (print (.join "---" (repeat "+" (inc (len board))))))
        
        (print)
        (separator)
        (for [(, x y) board]
          (print (.join "   " (repeat "|" (inc y))) :end "")
          (print " X " :end "")
          (print (.join "   " (repeat "|" (- (len board) y))))
          (separator)))
    (print "No solution found")))

(defn place-queen [location]
  "create place queen operator"
  {:action (fn [state]
             (if state
               (+ state (, location))
               (, location)))
   :desc (.format "place queen at {0}" location)})

(defn solve [n]
  "solve n queens problem"

  (defn goal? [queens]
    "has the solution been found?"
    (and (= (len queens) n)
         (not (conflicts? queens))))

  (defn operators [queens]
    "give list of valid operators for given state"
    (setv current (len queens))

    (->> (map (fn [x]
                (, current x))
              (range n))
         (filter (fn [x]
                   (not (can-take-any? x queens))))
         (map place-queen)))

  (setv solver (depth-first-solver :is-goal goal?
                                   :operators operators
                                   :is-identical identical?))
  (-> (solver (, ))
      (display)))
