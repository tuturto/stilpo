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

(defn assumptions-of-node [])

(defn create-jtms [title node-str-fn check-contradictions contradiction-fn
                   enqueue-fn]
  "create justification based truth maintenance system"
  {:title title
   :node-str-fn node-str-fn
   :check-contradictions check-contradictions
   :contradiction-fn contradiction-fn
   :enqueue-fn enqueue-fn
   :node-counter 0
   :justification-counter 0
   :nodes []
   :justifications []})

(defn tms-title [tms &optional [title 'no-value]]
  "get or set tms title"
  (when (not (= title 'no-value))
    (assoc tms :title title))
  (:title tms))

(defn tms-check-contradictions [tms &optional [value 'no-value]]
  "get or set tms contradiction checking"
  (when (not (= value 'no-value))
    (assoc tms :check-contradictions value))
  (:check-contradictions tms))

(defn tms-node-str-fn [tms &optional [str-fn 'no-value]]
  "get or set tms node str"
  (when (not (= str-fn 'no-value))
    (assoc tms :node-str-fn value))
  (:node-str-fn tms))

(defn default-node-str [node]
  "default function to display node as string"
  (str (node-datum node)))

(defn tms-contradiction-fn [tms &optional [contra-fn 'no-value]]
  "get or set contradiction fn"
  (when (not (= contra-fn 'no-value))
    (assoc tms :contradiction-fn contra-fn))
  (:contradiction-fn tms))

(defn tms-enqueue-fn [tms &optional [enqueue-fn 'no-value]]
  "get or set enqueue fn"
  (when (not (= enqueue-fn 'no-value))
    (assoc tms :enqueue-fn enqueue-fn))
  (:enqueue-fn tms))

(defn create-node [tms datum &optional [assumption? False] 
                   [contradiction? False]]
  "create node in tms"
  (assoc tms :node-counter (inc (:node-counter tms)))
  (setv new-node {:type 'node
                  :tms tms
                  :index (:node-counter tms)
                  :datum datum
                  :label 'out
                  :support None
                  :justifications []
                  :consequences []
                  :mark None
                  :in-rules []
                  :out-rules []
                  :assumption? is-assumption ; True / False / 'default
                  :contradiction? is-contradiction})
  (.append (:nodes tms) new-node)
  new-node)

(defn assume-node [node]
  "convert node to assumption and enable it"
  (assoc node :assumption? True)
  (assoc node :support None)
  (enable-assumption node)
  (check-contradictions (node-tms node)))

(defn turn-justification-in [justification &optional [skip-rules False]]
  "turn justification in if all antecedents are in"
  (assert (= (:type justification) 'justification))
  (when (and (in-node? justification)
             (out-node? (just-consequence justification)))   
    (turn-node-in (just-consequence justification) justification skip-rules)))

(defn turn-justification-out [justification]
  (assert (= (:type justification) 'justification))
  "turn justification out if not all antecedents are in")

(defn turn-node-in [node justification &optional [skip-rules False]]
  "turn node in because of justification"
  (assert (= (:type node) 'node))
  (assoc node :label 'in)
  (assoc node :support justification)
  (when (and (not skip-rules)
             (node-in-rules node))
    (for [rule (node-in-rules node)]
      ((:enqueue-fn (node-tms node)) rule))
    (assoc node :in-rules []))
  (for [just (:justifications node)]
    (turn-justification-in just skip-rules)))

(defn recursive-nodes-supported [justification]
  "get list of all nodes that are supported by given justification"
  (assert (= (:type justification) 'justification))
  (let [consequence (:consequence justification)
        supporting-justification (:support consequence)
        consequences (:consequences consequence)]
    (if (and (= supporting-justification justification)
             (is-not consequence None))
      (let [result [consequence]]
        (.extend result
                 (.from-iterable chain (map recursive-nodes-supported 
                                            consequences)))
        result)
      [])))

(defn enable-assumption [node]
  "enable assumption"
  (assert (:assumption? node))
  (let [old-value (:label node)]
    (assoc node :label 'in)
    (assoc node :support 'enabled-assumption)
    (if (= old-value 'out)
      (for [justification (:consequences node)]
        (turn-justification-in justification))
      (for [justification (:consequences node)]
        (turn-justification-out node))))
  (check-contradictions (node-tms node)))

(defn retract-assumption [node]
  "retract assumption"
  (assert (:assumption? node))
  (assoc node :label 'out)

  (setv all-supported-nodes (.from-iterable chain (map recursive-nodes-supported
                                                       (:consequences node))))
  (setv out-queue [])
  (for [out-node all-supported-nodes]
    (assoc out-node :label 'out)
    (assoc out-node :support None))
  
  (for [out-node all-supported-nodes]
    (setv supporting (list (filter in-node? (:justifications out-node))))
    (if supporting
      (do (assoc out-node :support (first supporting))
          (assoc out-node :label 'in))
      (.append out-queue out-node)))

  (for [out-node out-queue]
    (when (out-node? out-node)
      (setv supports (list (filter in-node? (:justifications out-node))))
      (when supports
        (turn-node-in out-node (first supports) True))))

  ;; process out-rules for nodes that were left 'out
  (check-contradictions (node-tms node)))

(defn in-node? [node]
  "is node believed?"
  (cond [(= (:type node) 'node) (= (:label node) 'in)]
        [(= (:type node) 'justification) (all (map in-node? 
                                                   (just-antecedents node)))]
        [True (assert False "Unknown node type")]))

(defn out-node? [node]
  "is node not believed?"  
  (not (in-node? node)))

(defn contradiction? [node]
  "is node marked as contradiction"
  (:contradiction? node))

(defn create-justification [tms informant consequence antecedents]
  "create justification in tms"
  (assoc tms :justification-counter (inc (:justification-counter tms)))
  {:type 'justification
   :index (:justification-counter tms)
   :informant informant
   :consequence consequence
   :antecedents antecedents
   :node-str None})

(defn just-antecedents [node]
  "get antacedents of justification"
  (:antecedents node))

(defn just-consequence [node]
  "get consequence of justification"
  (:consequence node))

(defn just-informant [node &optional [value 'no-value]]
  "get or set informant of justification"
  (when (not (= value 'no-value))
    (assoc node :informant value))
  (:informant node))

(defn justify-node [informant consequence antecedents]
  (let [tms (node-tms consequence)
        justification (create-justification tms
                                            informant
                                            consequence
                                            antecedents)] 
    (for [antecedent antecedents]
      (.append (:consequences antecedent) justification))
    (.append (:justifications consequence) justification)
    (.append (:justifications tms) justification)
    (turn-justification-in justification)
    (check-contradictions tms)))

(defn check-contradictions [tms]
  "check for contradictions"
  (when (:check-contradictions tms)
    (setv handler (:contradiction-fn tms))    
    (->> (filter (fn [x]
                   (and (contradiction? x)
                        (in-node? x))) (:nodes tms))
         (map handler)
         (list))))

(defn make-contradiction [])

(defn supporting-justification-for-node [node]
  "supporting justification for node"
  (:support node))

(defn assumptions-of-node [node]
  "list of all assumptions of node"
  (setv res [])
  (setv processed-nodes [])
  (setv justifications (:justifications node))
  
  (while justifications
    (setv assumptions (->> (.from-iterable chain (map (fn [just]
                                                        (:antecedents just))
                                                      justifications))
                           (filter (fn [x]
                                     (:assumption? x)))
                           (list)))
    (setv justifications [])
    (for [candidate assumptions]
      (when (not (in (:index candidate) processed-nodes))
        (.append processed-nodes (:index candidate))
        (.append res candidate)
        (.extend justifications (:justifications candidate)))))

  res)

(defn node-datum [node]
  "get datum of node"
  (:datum node))

(defn node-tms [node]
  "get tms of node"
  (:tms node))

(defn node-in-rules [node]
  "in rules of node"
  (:in-rules node))

(defn node-out-rules [node]
  "out rules of node"
  (:out-rules node))
