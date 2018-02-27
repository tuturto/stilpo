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

(require [hy.extra.anaphoric [ap-each]])

(defn create-tre [tre-name &optional [debug False]]
  "create a new tiny rule engine"
  {:title tre-name
   :frames (, (create-frame "root"))
   :assertion-queue []
   :assumption-queue []
   :rule-queue []
   :a-rule-queue []
   :debug debug})

(defn create-frame [desc]
  {:description desc
   :assertions []
   :rules []})

(defmacro/g! assert! [tre assertion &optional bindings]
  "assert a fact"
  `(do 
    (setv ~g!quoted (quote ~assertion))
    (if (debug ~tre)
      (if ~bindings
        (print "asserting" (fill-assertion ~g!quoted ~bindings))
        (print "asserting" (HyExpression ~g!quoted) "<no bindings>")))
    (setv ~g!env (if ~bindings
                   ~bindings
                   {}))
    (setv ~g!filled-assertion (fill-assertion ~g!quoted ~g!env))
    (when (not (or (assertion-defined? ~tre ~g!filled-assertion)
                   (assertion-queued? ~tre ~g!filled-assertion)))
      (.append (assertion-queue ~tre) ~g!filled-assertion))))

(defmacro/g! rule [tre pattern &rest body]
  "add new rule to tiny rule engine"
  (if (symbol? (last body))
    (setv bindings (last body))
    (setv bindings None))
  
  (setv rules (list (map (fn [x]
                           `(queue-rule ~tre (new-rule (quote ~pattern)
                                                       (fn [~g!bindings] (~@x ~g!bindings))
                                                       ~bindings)
                                        False))
                         (filter (fn [x] (not (symbol? x)))
                                 body))))
  `(do
    ~@rules))

(defn run [tre]
  "process tre until all queues are empty"
  (while (or (rule-queue tre)
             (assertion-queue tre))
    (process-assertion-queue tre)
    (process-rule-queue tre)))

(defn true? [tre assertion]
  (in assertion (assertions tre)))

(defn show [tre symbol]
  "show all assertions relating to given symbol"
  (ap-each (assertions tre)
           (when (in symbol it)
             (print (.join " " it))))
  (ap-each (assertion-queue tre)
           (when (in symbol it)
             (print (.join " " it)))))

(defn title [tre]
  "title of tre"
  (:title tre))

(defn debug [tre &optional [value :none]]
  "debug mode of tre"
  (if-not (= value :none)
          (assoc tre :debug value))
  (:debug tre))

(defn show-tre [tre]
  "show tre state for debugging purposed"
  (print "****" (title tre) "****")
  (print "assertions:" (assertions tre))
  (print "assertion queue:" (assertion-queue tre))
  (print "rules:" (rules tre))
  (print "rule queue:" (rule-queue tre))
  (print))

(defn assertions [tre]
  "get assertions of tre"
  (.from-iterable chain (list-comp (:assertions x) [x (frames tre)])))

(defn assertion-queue [tre]
  "get assertion queue of tre"
  (:assertion-queue tre))

(defn assumption-queue [tre]
  "get assumption queue of tre"
  (:assumption-queue tre))

(defn rules [tre]
  "get rules of tre"
  (.from-iterable chain (list-comp (:rules x) [x (frames tre)])))

(defn add-rule [tre new-rule]
  "add new rule to database without queuing it"
  (.append (:rules (last (frames tre))) new-rule))

(defn rule-queue [tre]
  "get rule queue of tre"
  (:rule-queue tre))

(defn a-rule-queue [tre]
  "get assumption rule queue of tre"
  (:a-rule-queue tre))

(defn process-rule-queue [tre]
  "take first rule in queue and process it"
  (when (rule-queue tre)
    (setv processed-rule (.pop (rule-queue tre)))
    (add-rule tre processed-rule)
    (ap-each (assertions tre)
             (run-rule processed-rule it))))

(defn run-rule [rule assertion]
  "run rule for assertion"
  (setv bindings (unify assertion
                        (first rule)
                        (get rule 2)))
  (when bindings
    ((second rule) bindings)))

(defn queue-rule [tre new-rule assumption]
  "queue new rule into tre database"
  (if assumption
    (.append (a-rule-queue tre) new-rule)
    (.append (rule-queue tre) new-rule)))

(defn new-rule [pattern body-fn &optional bindings]
  "create new rule from pattern and body"
  (, pattern body-fn
             (if bindings
               (copy-bindings bindings)
               {})))


(defn process-assertion-queue [tre]
  "take first assertion in queue and process it"
  (when (assertion-queue tre)
    (setv new-assertion (.pop (assertion-queue tre)))
    (when (not (assertion-defined? tre new-assertion))
      (add-assertion tre new-assertion)
      (ap-each (rules tre)
               (run-rule it new-assertion)))))

(defn add-assertion [tre assertion]
  "add assertion into current frame of tre"
  (.append (:assertions (last (frames tre))) assertion))

(defn assertion-defined? [tre assertion]
  "check if given assertion is already defined"
  (in assertion (assertions tre)))

(defn assertion-queued? [tre assertion]
  "check if given assertion is already queued"
  (in assertion (assertion-queue tre)))


(defn unify [assertion pattern bindings]
  "unify assertion and pattern, return new bindings or None"
  (if (= (len assertion) (len pattern))
    (reduce unify-sym (zip assertion pattern)
            (copy-bindings bindings))))

(defn unify-sym [env (, assertion-sym pattern-sym)]
  "unify two symbols while respecting env"
  (cond [(is env None) None]
        [(= assertion-sym pattern-sym) env]
        [(and (var? pattern-sym)
              (in pattern-sym env)
              (= (get env pattern-sym) assertion-sym)) env]
        [(and (var? pattern-sym)
              (not (in pattern-sym env))
              (not (in assertion-sym (.values env)))) (add-binding env 
                                                                   pattern-sym 
                                                                   assertion-sym)]
        [True None]))

(defn fill-assertion [assertion bindings]
  "copy values of bindings into assertion"
  (defn get-value [sym]
    (if (in sym bindings)
      (get bindings sym)
      sym))
  (HyExpression (map get-value assertion)))

(defn add-binding [bindings sym value]
  "bind symbol to value"
  (assoc bindings sym value)
  bindings)

(defn var? [sym]
  "check if given symbol is variable"
  (= (first sym) "?"))

(defn copy-bindings [bindings]
  "create copy of bindings"
  (dict bindings))


(defn push-tre [tre desc]
  "pushes state of tre in stack and creates a new one"
  (assert (not (rule-queue tre)) "unprocessed rules in queue")
  (assert (not (assertion-queue tre)) "unprocessed assertions in queue")
  (if (debug tre)
    (print "Pushing to stack:" desc))  
  (assoc tre :frames (+ (frames tre) (, (create-frame desc)))))

(defn pop-tre [tre]
  "pops state of tre from stack, discarding one level of assumptions"
  (assert (not (rule-queue tre)) "unprocessed rules in queue")
  (assert (not (assertion-queue tre)) "unprocessed assertions in queue")
  (if (debug tre)
    (print "Popping from stack:" (frame-title tre)))
  (assoc tre :frames (tuple (butlast (frames tre)))))

(defn frame-title [tre]
  "get title of current frame"
  (:description (last (frames tre))))

(defn frames [tre]
  "get frames of tre"
  (:frames tre))

(defmacro/g! try-in-context [tre assertion &rest body]
  `(do (run ~tre)
       (push-tre ~tre (+ "assuming that " (.join " " (quote ~assertion))))
       (assert! ~tre ~assertion)
       (run ~tre)
       (setv ~g!res (do ~@body))
       (pop-tre ~tre)
       ~g!res))
