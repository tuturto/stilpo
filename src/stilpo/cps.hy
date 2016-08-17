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

(require hymn.dsl)
(require hy.contrib.anaphoric)
(import [copy [copy]])

(defmacro operator [name desc guard &rest action]
  `(defn ~name [state]
     (when ~guard
       {:action (fn [state]
                  ~@action)
        :desc ~desc})))

(defn valid-operators [state &rest operators]
  "given state and list of operator checks, return valid operators"
  (list (filter (fn [x]
                  (if x true false))
                (map (fn [x] (x state))
                     operators))))

(defn breadth-first-solver [goal? operators identical?]
  "create classical breadth first solver"
  (fn [state]
    "try to solve path from given path to goal and return (maybe path)."
    (setv solution nil)
    (setv queue [])
    (.append queue [{:state state}])

    (while queue
      (setv current-path (.pop queue))
      (setv current-state (:state (last current-path)))
      (setv possible-operators (operators current-state))
      (setv new-steps (list (ap-map {:action it
                                     :state ((:action it) current-state)}
                                    possible-operators)))
      (setv solution (ap-if (list (filter (fn [x] (goal? (:state x))) new-steps))
                            (do (.append current-path (first it))
                                (.clear queue)
                                current-path)
                            (.extend queue
                                     (ap-map (create-new-path current-path it)
                                             (remove-loops current-path new-steps identical?))))))
    solution))

(defn remove-loops [path steps pred]
  (filter (fn [x]
            (setv res true)
            (for [step path]
              (when (pred (:state step) (:state x))
                (setv res false)
                (break)))
            res)
          steps))

(defn create-new-path [path state]
  (setv new-path (copy path))
  (.append new-path state)
  new-path)
