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

;; what kind of weather it is?

(import [stilpo.jtms [create-jtms default-node-str create-node
                      justify-node in-node? out-node?
                      enable-assumption retract-assumption
                      recursive-nodes-supported node-datum
                      assumptions-of-node]])

(defn contradiction-handler [node]
  "handle contradictions"
  (print "*** contradiction ***" (node-datum node)))

(defn enqueue-handler [rule]
  "report new rule"
  (print "*** enqueued ***" rule))

(setv tms (create-jtms "weather planning" default-node-str
                       :check-contradictions True
                       :contradiction-fn contradiction-handler
                       :enqueue-fn enqueue-handler))

(setv sun-is-out (create-node tms "Sun is out" :is-assumption True))

(setv sky-is-cloudy (create-node tms "Sky is cloudy" :is-assumption True))

(setv warm-temperature (create-node tms "Temperature is warm" 
                                    :is-assumption True))

(setv cold-temperature (create-node tms "Temperature is cold"
                                    :is-assumption True))

(setv c1 (create-node tms "Weather is warm and cold"
                      :is-contradiction True))

(justify-node "Conflict in temperatures" c1 [warm-temperature cold-temperature])

(setv wear-sunglasses (create-node tms "Wear sunglasses"))

(justify-node "Sunny weather requires sunglasses" wear-sunglasses [sun-is-out])

(setv wear-jacket (create-node tms "Wear jacket"))

(justify-node "Cold temperature requires jacket" wear-jacket [cold-temperature])

(setv summer? (create-node tms "Is it summer?"))

(justify-node "Sunglasses in war weather means summer" summer? 
              [wear-sunglasses warm-temperature])

(print "should I wear sunglasses?" (in-node? wear-sunglasses))
(print "should I wear jacket?" (in-node? wear-jacket))
(print "is it summer?" (in-node? summer?))

(print "assuming sun and warm")
(enable-assumption sun-is-out)
(enable-assumption warm-temperature)

(print "should I wear sunglasses?" (in-node? wear-sunglasses))
(print "should I wear jacket?" (in-node? wear-jacket))
(print "is it summer?" (in-node? summer?))

;; conflict reporting

(print "assuming cold weather")
(enable-assumption cold-temperature)

(print "assumptions leading to conflict:")
(for [node (assumptions-of-node c1)]
  (print "  " (node-datum node)))


(print "restracting warm temperature")
(retract-assumption warm-temperature)

(print "should I wear sunglasses?" (in-node? wear-sunglasses))
(print "should I wear jacket?" (in-node? wear-jacket))
(print "is it summer?" (in-node? summer?))
