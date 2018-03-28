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

(require [hy.extra.anaphoric [ap-if ap-each ap-map]])

(import [copy [copy]]
        [heapq [heappush heappop]]
        random)

(defmacro operator [name desc guard &rest action]
  `(defn ~name [state]
     (when ~guard
       {:action (fn [state]
                  ~@action)
        :desc ~desc})))

(defn valid-operators [state &rest operators]
  "given state and list of operator checks, return valid operators"
  (list (filter (fn [x]
                  (if x True False))
                (map (fn [x] (x state))
                     operators))))

(defn breadth-first-solver [goal? operators identical?]
  "create classical breadth first solver"
  (solver (fn [queue] (.pop queue 0))
          goal? operators identical?
          (fn [queue coll]
            (.extend queue coll))))

(defn depth-first-solver [goal? operators identical?]
  "create classical depth first solver"
  (solver (fn [queue] (.pop queue))
          goal? operators identical?
          (fn [queue coll]
            (.extend queue coll))))

(defn best-first-solver [goal? operators identical? distance]
  "create classical best first solver"
  (solver closest-to-goal goal? operators identical?
          (fn [queue coll]
            (.extend queue coll))
          distance))

(defn a*-solver [goal? operators identical? distance distance-between]
  "create a* solver"
  (solver (fn [queue]
            (nth (heappop queue) 2))
          goal? operators identical?
          (fn [queue coll]
            (for [item coll]
              (heappush queue (, (:total-distance (last item)) (.random random) item))))
          distance distance-between))

(defn solver [next-path goal? operators identical? add-queue
              &optional distance distance-between]
  "create classical best first solver"
  (fn [state]
    "try to solve path from given path to goal and return path"
    (setv solution None)
    (setv queue [])
    (setv iteration 0)
    (if distance
      (add-queue queue [[{:state state
                         :distance-left (distance state)
                         :distance-so-far 0
                         :total-distance (distance state)}]])
      (add-queue queue [[{:state state}]]))
    
    (while queue
      (setv iteration (inc iteration))
      (setv current-path (next-path queue))
      (setv current-state (:state (last current-path)))
      (setv possible-operators (operators current-state))
      (setv new-steps (list (ap-map {:action it
                                     :state ((:action it) current-state)}
                                    possible-operators)))
      (cond [(and distance distance-between) (calculate-distances distance
                                                                  new-steps
                                                                  distance-between
                                                                  current-path)]
            [distance (calculate-distances distance new-steps)])
      (setv solution (ap-if (list (filter (fn [x] (goal? (:state x))) new-steps))
                            (do (.append current-path (first it))
                                (.clear queue)
                                current-path)
                            (add-queue queue
                                       (ap-map (create-new-path current-path it)
                                               (remove-loops current-path new-steps identical?))))))
    {:path solution
     :length (if solution
               (len solution)
               None)
     :iterations iteration}))

(defn closest-to-goal [queue]
  (setv res (last queue))
  (setv distance (:distance-left (last res)))
  (ap-each queue
           (do (setv new-distance (:distance-left (last it)))
               (when (and new-distance
                          (< new-distance distance))
                 (setv distance new-distance)
                 (setv res it))))
  (.remove queue res)
  res)

(defn shortest-path [queue]
  (setv res (first queue))
  (setv distance (:total-distance (last res)))
  (ap-each queue
           (do (setv new-distance (:total-distance (last it)))
               (when (< new-distance distance)
                 (setv distance new-distance)
                 (setv res it))))
  (.remove queue res)
  res)

(defn calculate-distances [distance-left steps &optional distance-between previous-path]
  (when steps
    (when previous-path
      (setv previous-step (last previous-path))
      (setv previous-state (:state previous-step)))

    (ap-each steps
             (do (assoc it :distance-left (distance-left (:state it)))
                 (when distance-between
                   (setv distance-so-far (+ (distance-between (:state it) previous-state)
                                            (:distance-so-far previous-step)))
                   (setv total-distance (+ distance-so-far (:distance-left it)))
                   (assoc it :distance-so-far distance-so-far)
                                      (assoc it :total-distance total-distance))))))

(defn remove-loops [path steps pred]
  (filter (fn [x]
            (setv res True)
            (for [step path]
              (when (pred (:state step) (:state x))
                (setv res False)
                (break)))
            res)
          steps))

(defn create-new-path [path state]
  (setv new-path (copy path))
  (.append new-path state)
  new-path)
