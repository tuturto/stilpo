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

(defn create-frog [id pad &optional red]
  "create new frog"
  {:id id
   :lilypad pad
   :red red})

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
  (let [frog-pairs (list (zip (range 13)
                        frog-positions))
        regular-frogs (list (map (fn [pair]
                                   (let [lilypad (get lilypads (second pair))
                                         frog (create-frog (first pair)
                                                           lilypad)]
                                     (assoc lilypad :frog frog)
                                     frog))
                                 (rest frog-pairs)))
        red-lilypad (get lilypads (second (first frog-pairs)))
        red-frog (create-frog (first (first frog-pairs))
                              red-lilypad
                              True)]
    (assoc (get lilypads (second (first frog-pairs))) :frog red-frog)
    {:lilypads lilypads
     :frogs (+ regular-frogs [red-frog])}))

(defn pretty-print [solution]
  "print out the solution"
  (let [path (:path solution)]
    (if path           
      (do (print (.format "solution found in {} iterations" (:iterations solution)))
       (for [step path]
         (when (in :action (.keys step))
           (print (:desc (:action step))))))
      (do (print "no solution found")))))

(defn goal? [state]
  "have we reached the end?"
  (and (= (len (:frogs state)) 1)
       (:red (first (:frogs state)))))

(defn lilypad-by-id [id state]
  (get (:lilypads state) id))

(defn frog-by-id [id state]
  (first (filter (fn [x]
                   (= (:id x) id))
                 (:frogs state))))

(defn move-frog [frog dest-connection]
  (let [start-lilypad-id (:id (:lilypad frog))
        destination-lilypad-id (:id (first dest-connection))
        in-between-lilypad-id (:id (second dest-connection))
        captured-frog-id (:id (:frog (second dest-connection)))
        frog-id (:id frog)]
    {:action (fn [state]
               (let [new-state (deepcopy state)
                     start-lilypad (lilypad-by-id start-lilypad-id new-state)
                     in-between-lilypad (lilypad-by-id in-between-lilypad-id new-state)
                     destination-lilypad (lilypad-by-id destination-lilypad-id new-state)
                     frog (frog-by-id frog-id new-state)
                     captured-frog (frog-by-id captured-frog-id new-state)
                     new-frogs (list (filter (fn [frog]
                                         (not (= (:id frog)
                                                 (:id captured-frog))))
                                       (:frogs new-state)))]
                 (assoc in-between-lilypad :frog None)
                 (assoc start-lilypad :frog None)
                 (assoc destination-lilypad :frog frog)
                 (assoc frog :lilypad destination-lilypad)
                 (assoc new-state :frogs new-frogs)
                 
                 new-state))
     :desc (.format "{} {} -> {}"
                    (if (:red frog) "red" "green")
                    start-lilypad-id
                    destination-lilypad-id)}))

(defn frog-moves [frog]
  "get valid moves for a frog"
  (let [lilypad (:lilypad frog)
        connections (:connections lilypad)
        valid-connections (filter (fn [pair]
                                    (and (is (:frog (first pair)) None)
                                         (is-not (:frog (second pair)) None)
                                         (not (:red (:frog (second pair)))))) 
                                  connections)
        moves (list (map (fn [pair]
                           (move-frog frog pair))
                         valid-connections))]
    moves))

(defn operators [state]
  "all valid operators and their descriptions for given state"  
  (list (->> (:frogs state)
             (map frog-moves)
             (filter (fn [moves]
                       (> (len moves) 0)))
             (.from-iterable chain))))

(defn identical? [state1 state2]
  "are two given states identical?"
  (when (= (len (:frogs state1))
           (len (:frogs state2)))
    (all (map (fn [frog]     
                (= (:id (:lilypad frog))
                   (:id (:lilypad (frog-by-id (:id frog) state1)))))
              (:frogs state2)))))

(setv frog-positions ["B2" "A1" "B1" "C1" "D1" "A3" "B4"])

(setv lilypad-ids ["A1" "A2" "A3" "A4" "A5"
                   "B1" "B2" "B3" "B4"
                   "C1" "C2"
                   "D1" "D2"])

(setv state (place-frogs (create-board)
                         frog-positions))

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
