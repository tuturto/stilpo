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

(defn unify [assertion pattern bindings unique-symbols]
  "unify assertion and pattern, return new bindings or None"

  (setv new-bindings (copy-bindings bindings))
  
  (defn valid-value? [env sym value]
    "check that symbol has unique value if required"
    (->> (filter (fn [sym-pair]
                   (in sym sym-pair))
                 unique-symbols)
         (map (fn [sym-list]
                (all (map (fn [unique-sym]
                            (or (= unique-sym sym)
                                (not (in unique-sym new-bindings))
                                (not (= (get new-bindings unique-sym)
                                        value))))
                          sym-list))))
         (all)))
  
  (defn unify-sym [env (, assertion-sym pattern-sym)]
    "unify two symbols while respecting env"
    (cond [(is env None) None]
          [(= assertion-sym pattern-sym) env]
          [(and (var? pattern-sym)
                (in pattern-sym env)
                (= (get env pattern-sym) assertion-sym)) env]
          [(and (var? pattern-sym)
                (not (in pattern-sym env))
                (valid-value? env pattern-sym assertion-sym)) (add-binding env 
                                                                           pattern-sym 
                                                                           assertion-sym)]
          [True None]))
  
  (if (= (len assertion) (len pattern))
    (reduce unify-sym (zip assertion pattern)
            new-bindings)))

(defn copy-bindings [bindings]
  "create copy of bindings"
  (dict bindings))

(defn var? [sym]
  "check if given symbol is variable"
  (= (first sym) "?"))

(defn add-binding [bindings sym value]
  "bind symbol to value"
  (assoc bindings sym value)
  bindings)
