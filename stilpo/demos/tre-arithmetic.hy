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

(import [stilpo.tre [create-tre run show]])
(require [stilpo.tre [rule assert! try-in-context]])

(setv tre (create-tre "Mission planning"))

(rule tre (speed is ?v)
      (rule tre (time is ?t)
            (setv ?d (* ?v ?t))
            (assert! tre (distance is ?d))))

(rule tre (distance is ?d)
      (rule tre (endurance is ?e)
            (if (> ?e ?d)
              (assert! tre (mission status: good))
              (assert! tre (mission status: bad)))))

(assert! tre (speed is 20))
(assert! tre (time is 10))

(run tre)
(show tre 'mission)

(try-in-context tre (endurance is 220)
                (show tre 'mission))

(try-in-context tre (endurance is 190)
                (show tre 'mission))
