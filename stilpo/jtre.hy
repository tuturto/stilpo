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

(import [stilpo.jtms [create-jtms create-node justify-node
                      default-node-str]])

;; --- main interface

(defn create-jtre [tre-name &optional [debug False]]
  "create a new tiny rule engine"
  {:title tre-name
   :jtms (create-jtms tre-name default-node-str
                      True None None) ;; contradiction-fn enqueue-fn
   :assumption-queue [] ;; name?
   :debug debug
   :datum-counter 0
   :rule-counter 0
   :assumption-counter 0})

(defn assert! [consequent informant antecedents])

(defn quiet-assert! [consequent informant antecedents])

(defn already-assumed? [])

(defn retract! [])

(defn contradiction [node])

;; --- exploring jtre

(defn in? [])

(defn out? [])

(defn why? [])

(defn assumptions-of [])

(defn well-founded-support [])

(defn show-justifications [])

(defn fetch [])

;; --- low level debug

(defn referent [])

(defn get-tms-node [])

(defn view-node [])

(defn show-datum [])

(defn get-datum [])

(defn get-just [])

;; --- internals

(defn create-datum [id jtre form tms-node assumption?]
  "create datum to be stored in jtre"
  {:id id
   :jtre jtre
   :form form
   :tms-node tms-node
   :assumption? assumption?})
