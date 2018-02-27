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

;; Is Alice grand parent of Charlie?

(require [stilpo.tre [assert! rule]])
(import [stilpo.tre [create-tre run show true? push-tre pop-tre]])

(setv tre (create-tre "family" :debug True))

(assert! tre (Alice is parent of Bob))
(assert! tre (Bob is parent of Charlie))

(rule tre (?x is parent of ?y)
      (assert! tre (?y is children of ?x)))

(rule tre (?x is parent of ?y)
      (rule tre (?y is parent of ?z)
            (assert! tre (?x is grand-parent of ?z))))

(rule tre (?x is grand-parent of ?y)
      (assert! tre (?y is grand-children of ?x)))

(run tre)

(show tre 'Alice)

(true? tre '(Alice is grand-parent of Charlie))
(true? tre '(Bob is grand-parent of Charlie))

(true? tre '(Charlie is children of Alice))

(show tre 'Bob)
(show tre 'Charlie)

;; Who is Daemon?

(push-tre tre "Assuming Daemon is really old")

(assert! tre (Daemon is parent of Alice))
(run tre)
(show tre 'Daemon)

(pop-tre tre)
(push-tre tre "Assuming Daemon is really young")

(assert! tre (Charlie is parent of Daemon))
(run tre)
(show tre 'Daemon)

;; Could Ezekiel be Grand-Parent of Charlie?

(try-in-context tre (Ezekiel is parent of Alice)
                (print (true? tre '(Ezekiel is grand-parent of Charlie))))

(try-in-context tre (Ezekiel is parent of Bob)
                (print (true? tre '(Ezekiel is grand-parent of Charlie)))) 

;; try-in-context supports multiple code statements

(try-in-context tre (Ezekiel is parent of Alice)
                (print "Ezekiel is grand-parent of Bob:"
                       (true? tre '(Ezekiel is grand-parent of Bob)))
                (print "Ezekiel is grand-parent of Charlie:"
                       (true? tre '(Ezekiel is grand-parent of Charlie))))
