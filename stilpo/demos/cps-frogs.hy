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

;; solving a ToaZZle puzzle with cps

(require [hy.contrib.walk [let]])

(import [copy [deepcopy]]
        [itertools [chain]]
        [stilpo.cps [breadth-first-solver depth-first-solver]])

(defn create-lilypad [id]
  "create new lilypad"
  {:id id
   :frog None
   :connections []})

(defn connect-lilypads [origin connections]
  "connect lilypad to its neighbours"
  (for [connection connections]
    (.append (:connections origin) connection)
    (.append (:connections (first connection)) (, origin (second connection)))))

(defn create-board []
  "create board for puzzle number 13"
  (let [lilypads (dict-comp id (create-lilypad id) [id lilypad-ids])]
    (defn connect [origin-id &rest connection-pairs]
      "helper to connect pad to neighbours"
      (connect-lilypads (get lilypads origin-id)
                        (list (map (fn [x] (, (get lilypads (first x))
                                              (get lilypads (second x)))) 
                                   connection-pairs))))
    (connect "A1" (, "A2" "B1") (, "A3" "B2") (, "A5" "C1"))
    (connect "A2" (, "A4" "B3") (, "A5" "D2"))    
    (connect "A3" (, "A4" "B4") (, "A5" "D1"))
    (connect "A4" (, "A5" "C2"))
    (connect "B1" (, "B2" "C1") (, "B3" "D2") (, "B4" "A5"))
    (connect "B2" (, "B3" "A5") (, "B4" "D1"))
    (connect "B3" (, "B4" "C2"))
    (connect "C1" (, "C2" "A5"))
    (connect "D1" (, "D2" "A5"))

    lilypads))

(defn place-frogs [lilypads frog-positions]
  "place frogs on board"
  (assoc (get lilypads (first frog-positions)) :frog "red")
  (for [position (rest frog-positions)]
    (assoc (get lilypads position) :frog "green"))
  lilypads)

(defn pretty-print [solution]
  "print out the solution"
  (let [path (:path solution)]
    (if path           
      (do (print (.format "solution found in {} iterations" (:iterations solution)))
       (for [step path]
         (when (in :action (.keys step))
           (print (:desc (:action step))))))
      (do (print (.format "no solution found in {} iterations" (:iterations solution)))))))

(defn goal? [state]
  "have we reached the end?"
  (not (list (filter (fn [lilypad]
                       (= (:frog lilypad) "green"))
                     (.values state)))))

(defn move-frog [source connection]
  (let [start-lilypad-id (:id source)
        destination-lilypad-id (:id (first connection))
        in-between-lilypad-id (:id (second connection))
        frog (:frog source)]
    {:action (fn [state]
               (let [new-state (deepcopy state)
                     start-lilypad (get new-state start-lilypad-id)
                     in-between-lilypad (get new-state in-between-lilypad-id)
                     destination-lilypad (get new-state destination-lilypad-id)]
                 (assoc in-between-lilypad :frog None)
                 (assoc start-lilypad :frog None)
                 (assoc destination-lilypad :frog frog)
                 
                 new-state))
     :desc (.format "{} {} -> {}"
                    frog
                    start-lilypad-id
                    destination-lilypad-id)}))

(defn frog-moves [lilypad]
  "get valid moves for a frog on a given lilypad"
  (if (:frog lilypad)
    (let [connections (:connections lilypad)
          valid-connections (filter (fn [pair]
                                      (and (is (:frog (first pair)) None)
                                           (is-not (:frog (second pair)) None)
                                           (not (= "red"
                                                   (:frog (second pair)))))) 
                                    connections)
          moves (list (map (fn [connection]
                             (move-frog lilypad connection))
                           valid-connections))]
      moves)
    []))

(defn operators [state]
  "all valid operators and their descriptions for given state"  
  (list (->> (.values state)
             (map frog-moves)
             (filter (fn [moves]
                       (> (len moves) 0)))
             (.from-iterable chain))))

(defn identical? [state1 state2]
  "are two given states identical?"
  (let [lilypad-pairs (zip (.values state1)
                           (.values state2))]
    (all (map (fn [pair]
                (= (:frog (first pair))
                   (:frog (second pair))))
              lilypad-pairs))))

(setv lilypad-ids ["A1" "A2" "A3" "A4" "A5"
                   "B1" "B2" "B3" "B4"
                   "C1" "C2"
                   "D1" "D2"])

(setv d-solve (depth-first-solver :is-goal goal?
                                  :operators operators
                                  :is-identical identical?))

(setv levels [(, "Level 1" ["D1" "A1" "C1" "A5"])
              (, "Level 17" ["C2" "A1" "B1" "C1" "D2" "B2" "A5" "B3" "D1" "B4"])              
              (, "Level 35" ["D1" "A1" "B1" "C1" "D2" "B2" "A5" "B3" "C2" "B4" "A4"])              
              (, "Level 40" ["B2" "A1" "B1" "A2" "C1" "D2" "A5" "D1" "C2" "A3" "B4" "A4"])])

(for [level levels]
  (let [frogs (second level)
        state (place-frogs (create-board)
                           frogs)]
    (print (first level))
    (print (.format "Frogs at {0} (red), {1}"
                    (first frogs)
                    (.join ", " (rest frogs))))
    (-> (d-solve state)
        (pretty-print))
    (print)))
