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

(import [copy [copy]])

(defn unify [assertion pattern bindings unique-symbols]
  (defn unify-sym [state sym]
    "unify single symbol"
    (setv pattern-sym (first (:pattern state)))
    (setv wildcard (:wildcard state))
    (setv bindings (:bindings state))
    (setv unique-symbols (:unique-symbols state))
    
    (cond [(:mismatch state) state]

          [(starting-wildcard? sym (:pattern state) (:wildcard state))
           (dict state #**{:wildcard (, pattern-sym `(~sym))
                           :pattern (list (rest (:pattern state)))})]

          [(valid-wildcard? sym (:pattern state) (:wildcard state))
           (dict state #**{:wildcard (, (first (:wildcard state))
                                        (+ (second (:wildcard state)) `(~sym)))})]

          [(ending-wildcard? sym (:pattern state) (:wildcard state) bindings unique-symbols)
           (unify-sym (dict state #**{:wildcard (, '())
                                      :bindings (dict (:bindings state)
                                                      #**{(first (:wildcard state))
                                                          (second (:wildcard state))})})
                      sym)]
          
          [(and (matching-symbols? sym pattern-sym)
                (not (first (:wildcard state))))
           (dict state #**{:pattern (list (rest (:pattern state)))})]
          
          [(and (valid-joker? sym pattern-sym bindings unique-symbols) 
                (not (first (:wildcard state))))
           (dict state #**{:wildcard []
                           :pattern (list (rest (:pattern state)))
                           :bindings (dict (:bindings state)
                                           #**{pattern-sym sym})})]

          [True (dict state #**{:mismatch True})]))
  
  (setv res (reduce unify-sym assertion {:pattern pattern
                                         :wildcard (, None '())
                                         :bindings bindings
                                         :mismatch False
                                         :unique-symbols unique-symbols}))

  (when (first (:wildcard res))
    (if (and (not (:pattern res))
             (valid-value? (:bindings res)
                           (first (:wildcard res))
                           (second (:wildcard res))
                           (:unique-symbols res)))
      (setv res (dict res #**{:bindings (dict (:bindings res)
                                              #**{(first (:wildcard res))
                                                  (second (:wildcard res))})}))
      (setv res (dict res #**{:mismatch True}))))
  
  (if (not (:mismatch res))
    (:bindings res)
    None))

(defn fill-assertion [assertion bindings]
  "copy values of bindings into assertion"
  (defn get-value [sym]
    (if (in sym bindings)
      (get bindings sym)
      sym))
  (HyExpression (flatten (map get-value assertion))))

(defn starting-wildcard? [sym pattern wildcard]
  (and pattern
       (not (first wildcard))
       (wildcard-var? (first pattern))))

(defn valid-wildcard? [sym pattern wildcard]
  (or (and pattern
           (first wildcard)
           (not (= sym (first pattern))))
      (and (not pattern)
           (first wildcard))))

(defn ending-wildcard? [sym pattern wildcard bindings unique-symbols]
  (and (first wildcard)
       pattern
       (= sym (first pattern))
       (valid-value? bindings 
                     (first wildcard)
                     (second wildcard)
                     unique-symbols)))

(defn matching-symbols? [assert-sym pattern-sym]
  (and pattern-sym
       (not (var? pattern-sym))
       (= assert-sym pattern-sym)))

(defn valid-joker? [assert-sym pattern-sym bindings unique-symbols]
  (and (any-var? pattern-sym)
       (or (and (in pattern-sym bindings)
                (= (get bindings pattern-sym)
                   assert-sym))           
           (and (not (in pattern-sym bindings))
                (valid-value? bindings pattern-sym assert-sym unique-symbols)))))

(defn valid-value? [bindings sym value unique-symbols]
  "check that symbol has unique value if required"
  (->> (filter (fn [sym-pair]
                 (in sym sym-pair))
               unique-symbols)
       (map (fn [sym-list]
              (all (map (fn [unique-sym]
                          (or (= unique-sym sym)
                              (not (in unique-sym bindings))
                              (not (= (get bindings unique-sym)
                                      value))))
                        sym-list))))
       (all)))

(defn copy-bindings [bindings]
  "create copy of bindings"
  (dict bindings))

(defn var? [sym]
  "check if given symbol is variable"  
  (or (any-var? sym)
      (wildcard-var? sym)))

(defn any-var? [sym]
  "is given symbol is any variable"
  (and sym
       (= (first sym) "?")))

(defn wildcard-var? [sym]
  "is given symbol wildcard variable"
  (and sym
       (= (first sym) "*")))


