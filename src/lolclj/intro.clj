(ns lolclj.intro)

; On the question of a literal translation vs. idiomatic Clojure, I am taking
; a mixed approach. I have decided to begin by generally favoring a more
; literal interpretation of the LoL examples, and then progressing towards
; idiomatic Clojure as I learn.

; Introductory Chapters 2-4. 

; Chapter 2
; guess my number, let

; a single ref rather than global variables.
(def game (ref {:small 1 :big 100}))

(defn guess-my-number []
  (let [{:keys [small big]} @game]
    (bit-shift-right (+ small big) 1)))

(defn smaller []
  (dosync
   (alter game assoc :big (dec (guess-my-number))))
  (guess-my-number))

(defn bigger []
  (dosync
   (alter game assoc :small (inc (guess-my-number))))
  (guess-my-number))

(defn start-over []
  (dosync (ref-set game {:small 1 :big 100}))
  (guess-my-number))

; Differences with Common Lisp (surprising and otherwise)

; flet -> letfn
(letfn [(f [n] (+ n 10))
        (g [n] (- n 3))]
  (f (g 5)))

; labels -> also letfn
(letfn [(a [n] (+ n 5))
        (b [n] (+ (a n) 6))]
  (b 10))

; Chapter 3

; symbols are case sensitive
(= 'foo 'Foo)
; false

; expt is not in clojure core
(use '[clojure.math.numeric-tower :only [expt]])
(expt 53 53)
; 24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373N

; println returns nil
(println "Tutti Frutti")
; | Tutti Frutti
; nil

(first '(pork beef chicken))
; pork

(rest '(pork beef chicken))
; (beef chicken)

; cdr -> rest
(rest '())
; ()

(next '())
; nil

; Chapter 4

; () is true; only false and nil are false
(if '()
  'i-am-true
  'i-am-false)

; not idiomatic Clojure:
(defn my-length-bad [coll]
  (if coll
    (inc (my-length-bad (next coll)))
    0))

; recursive collection length
(defn my-length [coll]
  (loop [c coll len 0]
    (if (empty? c)
      len
      (recur (next c) (inc len)))))

; foop -> foo?
(if (odd? 5)
  'odd-number
  'even-number)

; progn -> do, when -> when, unless -> when-not

(if (odd? 5)
  (do (println "number was odd")
      'odd-number))

(when (odd? 5)
  (println "number was odd")
  'odd-number)

(when-not (odd? 4)
  (println "number was not odd")
  'even-number)

; cond, unlike progn, requires a (do ...) form
(defn pudding-eater [person]
  (cond (= person 'henry) (do (println "curse you clojure alien - you ate my pudding")
                              'stupid-clojure-alien)
        (= person 'johnny) (do (println "i hope you choked on my pudding johnny")
                               'useless-old-johnny)
        :else (println "why you eat my pudding stranger ?")))

(pudding-eater 'johnny)
; | i hope you choked on my pudding johnny
; useless-old-johnny
(pudding-eater 'henry)
; | curse you clojure alien - you ate my pudding
; stupid-clojure-alien
(pudding-eater 'george-clooney)
; | why you eat my pudding stranger ?
; nil

; case version, default can be final stand-alone expression
(defn pudding-eater2 [person]
  (case person
    henry (do (println "curse you clojure alien - you ate my pudding")
              'stupid-clojure-alien)
    johnny (do (println "i hope you choked on my pudding johnny")
               'useless-old-johnny)
    (println "why you eat my pudding stranger ?")))

; member
(defn member [n coll]
  (drop-while (complement #(= n %)) coll))

(member 1 [3 4 1 5])
;(1 5)

(member 9 [3 4 1 5])
; ()

; find-if
(defn find-if [pred coll]
  (first (filter pred coll)))

(find-if odd? [2 4 5 6])
; 5

(find-if odd? [2 4 6 8])
; nil

; symmetry break
(find-if nil? [2 4 nil 6])
; nil

; equality. Just use = for the most part, though == is useful for numbers:
(== 1 1.0)
; true

(= 1 1.0)
; false

; for caseless string compare fall back to Java:
(.equalsIgnoreCase "foo" "Foo")
; true
