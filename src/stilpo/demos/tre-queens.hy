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

;; solve 8 queens problem with tiny rule engine

(require [stilpo.tre [assert! rule]])
(import [copy [copy]]
        [stilpo.tre [create-tre run show true? push-tre pop-tre]])

(defn build-tre []
  "build tre for puzzle"
  (setv tre (create-tre "n queens"))

  (rule tre (queen ?q1 is placed on row ?r)
      (rule tre (queen ?q2 is placed on row ?r)
            (unique ?q1 ?q2)
            (assert! tre (warning queen ?q1 can be taken))))

  (rule tre (queen ?q1 is placed on row ?r1)
        (rule tre (queen ?q2 is placed on row ?r2)
              (unique ?q1 ?q2)
              (if (= (abs (- ?r2 ?r1))
                     (abs (- ?q2 ?q1)))
                (assert! tre (warning queen ?q1 can be taken)))))

  (run tre)

  tre)

(defn build-choice-sets [n]
  "build choice sets for queens"
  (list-comp 
   (list-comp `(queen ~queen is placed on row ~row) [row (range n)])
   [queen (range n)]))

(defn solve [tre choice-sets]
  "solve n queens problem"
  (setv solution False)
  (setv current-choices (copy (first choice-sets)))

  (while (and (not (fetch tre 'warning))
              (not solution)
              current-choices)    
    (setv choice (.pop current-choices))
    (if (not (try-in-context tre choice
                             (fetch tre 'warning)))
      (do (push-tre tre "placing queen")
          (assert! tre choice)
          (run tre)
          (if (list (rest choice-sets))
            (if (solve tre (list (rest choice-sets)))
              (setv solution True)
              (pop-tre tre))
            (setv solution True)))))
  
  (when solution tre))

(defn display [tre]
  "display board"
  (setv queens (sorted (map (fn [x] (, (get x 1)
                                       (get x 6)))
                            (fetch tre 'queen))
                       :key (fn [x] (second x))))
  
  (defn separator []
    "print separator line"
    (print (.join "---" (repeat "+" (inc (len queens))))))

  (print)
  (separator)
  (for [(, x y) queens]
    (print (.join "   " (repeat "|" (inc x))) :end "")
    (print " X " :end "")
    (print (.join "   " (repeat "|" (- (len queens) x))))
    (separator)))

(-> (solve (build-tre)
           (build-choice-sets 8))
    (display))

