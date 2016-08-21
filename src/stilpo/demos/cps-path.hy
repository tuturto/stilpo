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

(require hy.contrib.anaphoric)
(require stilpo.cps)

(import [math [sqrt]]
        [stilpo.cps [breadth-first-solver depth-first-solver best-first-solver
                     a*-solver valid-operators]])

(defn create-maze [&rest rows]
  "create maze"
  (setv res {})
  (setv row-num 0)
  (for [row rows]
    (do (setv column-num 0)
        (for [cell row]          
          (do (cond [(= cell "x") (assoc res (, column-num row-num) "x")]
                    [(= cell " ") (assoc res (, column-num row-num) " ")]
                    [(= cell "S") (do (assoc res (, column-num row-num) " ")
                                      (setv start (, column-num row-num)))]
                    [(= cell "G") (do (assoc res (, column-num row-num) " ")
                                      (setv goal (, column-num row-num)))])
              (setv column-num (inc column-num))))
        (setv row-num (inc row-num))))
  {:maze res
   :location start
   :goal goal})

(defn pretty-print [solution]
  (setv path (:path solution))
  (if path
    (do (setv path-locations (list-comp (:location (:state elem)) [elem path]))
        (setv maze (:maze (:state (first path))))
        (for [y (range 10)]
          (for [x (range 16)]
            (cond [(in (, x y) path-locations) (print "." :end "")]
                  [(= (get maze (, x y)) "x") (print "x" :end "")]
                  [true (print " " :end "")]))
          (print ""))
        (print "path length:" (:length solution))
        (print "number of iterations:" (:iterations solution)))
    (do (print "no solution found")
        (print "number of iterations:" (:iterations solution)))))


(def state
  (create-maze "xxxxxxxxxxxxxxxx"
               "x     x        x"
               "x  x  x Sx  x  x"
               "x     x  x     x"
               "x  x xx  x xxx x"
               "x  x x       x x"
               "x  x xxx   xxx x"
               "x Gx   xxxxx   x"
               "x   x          x"
               "xxxxxxxxxxxxxxxx"))

(defn goal? [state]
  "have we reached goal?"
  (= (:location state) (:goal state)))

(defn distance-left [state]
  "how far this state is from goal"
  (let [[start (:location state)]
        [end (:goal state)]
        [dx (abs (- (first start) (first end)))]
        [dy (abs (- (second start) (second end)))]]
    (sqrt (+ (* dx dx) (* dy dy)))))

(defn distance-between [state1 state2]
  "how far between two states"
  (let [[start (:location state1)]
        [end (:location state2)]
        [dx (abs (- (first start) (first end)))]
        [dy (abs (- (second start) (second end)))]]
    (sqrt (+ (* dx dx) (* dy dy)))))

(defn operators [state]
  "all valid operators for given state and their descriptions"
  (list (filter (fn [x]
                  (if x true false))
                (map (fn [x] (x state))
                     [move-north? move-east? move-west? move-south?]))))

(defn identical? [state1 state2]
  "are two given states identical?"
  (= (:location state1)
     (:location state2)))

(defn move [state new-location description]
  "helper function to create operator description pair"
  (let [[maze (:maze state)]]
    (if (and (in new-location maze)
             (= (get maze new-location) " "))
      {:action (fn [state]
                 {:maze (:maze state)
                  :goal (:goal state)
                  :location new-location})
       :desc (.format "{0}: {1}"
                      description
                      new-location)}
      false)))

(defn move-north? [state]
  "operator to move north"
  (let [[(, x y) (:location state)]]
    (move state (, x (dec y)) "move north")))

(defn move-east? [state]
  "operator to move east"
  (let [[(, x y) (:location state)]]
    (move state (, (inc x) y) "move east")))

(defn move-south? [state]
  "operator to move south"
  (let [[(, x y) (:location state)]]
    (move state (, x (inc y)) "move south")))

(defn move-west? [state]
  "operator to move west"
  (let [[(, x y) (:location state)]]
    (move state (, (dec x) y) "move west")))

(def b-solve (breadth-first-solver :is-goal goal?
                                   :operators operators
                                   :is-identical identical?))

(def d-solve (depth-first-solver :is-goal goal?
                                 :operators operators
                                 :is-identical identical?))

(def best-solve (best-first-solver :is-goal goal?
                                   :distance distance-left
                                   :operators operators
                                   :is-identical identical?))

(def a* (a*-solver :is-goal goal?
                   :distance-left distance-left
                   :distance-between distance-between
                   :operators operators
                   :is-identical identical?))

(print "\nsolving a* first")
(-> (a* state)
    (pretty-print))

(print "\nsolving maze best first")
(-> (best-solve state)
    (pretty-print))

(print "\nsolving maze breadth first")
(-> (b-solve state)
    (pretty-print))

(print "\nsolving maze depth first")
(-> (d-solve state)
    (pretty-print))

