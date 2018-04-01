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

;; simple chat bot

(import random
        [stilpo.tools [unify fill-assertion]])

(setv rules (, (, '(I need *x)
                   (, '(Why do you say, you need *x ?)
                       '(What would it mean to you, if you got *x ?)
                       '(Would having *x make you happy ?)))
               (, '(Im not *x)
                   (, '(Image that you are *x how does that make you feel?)
                       '(Would you like to be *x ?)))
               (, '(Im *x)
                   (, '(Are you happy when you are *x ?)
                       '(Would your world be different if you were not *x ?)))
               (, '(I like *x)
                   (, '(Everyone of us has likes and dislikes. Why do you like *x ?)
                       '(What do you like about *x ?)
                       '(Have you always liked *x ?)))
               (, '(I hate *x)
                   (, '(How does hating *x make you feel?)
                       '(Why do you hate *x ?)
                       '(What do you think is the reason for hating *x ?)))
               (, '(How are you?)
                   (, '(I am really well, thank you for asking)
                       '(I could not be better, how about you?)
                       '(Life is good, hopefully yours too)))
               (, '(Hello)
                   (, '(Hello, what would you like to talk about?)
                       '(Greetings, nice to see you)
                       '(Hello, do you have something on your mind?)
                       '(Greetings, how are you?)))
               (, '(*x your name *y)
                   (, '(Names are not important, or do you think otherwise?)
                       '(Do you think knowing true name gives you power over something?)))
               (, '(yes)
                   (, '(What makes you agree with that?)
                       '(Imagine the opposite, how would you see the situation then?)
                       '(Would your younger self have same view?)))
               (, '(no)
                   (, '(You seem very sure of yourself. Have it always been like that?)
                       '(Consider opposite, what kind of world would you be in then?)))
               (, '(?x)
                   (, '(You are not very talkative today)
                       '(Could you elaborate that a bit?)))
               (, '(*x)
                   (, '(Please, elaborate)
                       '(Interesting, please continue)))))

(defn discuss []
  "main loop for the discussion"
  (setv keep-going True)
  (while keep-going
    (setv query (input "?: "))
    (if (= query "bye")
      (setv keep-going False)
      (-> query
        (encode)
        (read-str)
        (find-match)
        (reply)
        (print))))
  (print "bye bye"))

(defn encode [query]
  "strip punctuation and create s-expression looking string"
  (.format "({})" (-> (.replace query "'" "")
                      (.replace "," "")
                      (.replace "." "")
                      (.replace "?" "")
                      (.replace "." ""))))

(defn reply [query]
  "turn s-expression back to string"
  (.join " " (map str query)))

(defn find-match [query]
  "return random reply from first matching rule"
  (->> (map (fn [rule]
              (setv response (unify query (first rule) {} (,)))
              (when (is-not None response)
                (fill-assertion (.choice random (second rule))
                                response)))
            rules)
       (filter (fn [response]
                 (is-not None response)))
       (first)))

(discuss)
