(ns lolclj.textgame
  (:require [clojure.string]))

; Chapter 5, simple wizard's house text game.

; I'm diverging from a literal LoL translation here a bit. I've chosen to use
; maps, vectors, and keywords and string rather than association lists of
; symbols to represent the world, as these feel a bit more "Clojure-ish" and
; I want to practice using Clojure's built-in datastructures.

(def nodes
  {:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
   :garden      "You are in a beautiful garden. There is a well in front of you."
   :attic       "You are in the attic. There is a giant welding torch in the corner."})

(def edges
  {:living-room [[:garden :west :door], [:attic :upstairs :ladder]]
   :garden      [[:living-room :east :door]]
   :attic       [[:living-room :downstairs :ladder]]})

(def objects #{:whiskey :bucket :chain :frog})

(def object-locations (ref {:whiskey :living-room
                            :bucket :living-room
                            :piece-of-pizza :kitchen
                            :cup-of-wine :kitchen
                            :chain :garden
                            :frog :garden}))

(def location (ref :living-room))

(defn describe-location [location nodes]
  ; could just do (nodes location) since location is a keyword
  (get nodes location))

(defn keyword-to-str [k]
  (apply str (next (str k))))

(defn describe-path [[_ direction via]]
  (format "There is a %s going %s from here."
          (keyword-to-str via)
          (keyword-to-str direction)))

(defn describe-paths [location edges]
  (clojure.string/join " " (map describe-path (location edges))))

(defn objects-at [location objects object-locations]
  (set (filter #(= location (get object-locations %)) objects)))

(defn describe-objects [location objects object-locations]
  (clojure.string/join " "
                       (map #(format "You see a %s on the floor." (keyword-to-str %))
                            (objects-at location objects object-locations))))

(defn look []
  (clojure.string/join " " [(describe-location @location nodes)
                            (describe-paths @location edges)
                            (describe-objects @location objects @object-locations)]))

(defn walk [direction]
  (let [next-edge (first (filter #(= direction (fnext %)) (@location edges)))]
    (if next-edge
      (do (dosync (ref-set location (first next-edge)))
          (look))
      "You cannot go that way.")))

(defn pickup [object]
  (if ((objects-at @location objects @object-locations) object)
    (do (dosync (alter object-locations assoc object :body))
        (format "you are now carrying the %s." (keyword-to-str object)))
    "you cannot get that."))

(defn inventory []
  (str "items- "
       (clojure.string/join " "
                            (map keyword-to-str (objects-at :body objects @object-locations)))))

(defn game-read []
  (let [cmd (read-string (str "(" (read-line) ")"))]
    (cons (first cmd) (map #(keyword %) (next cmd)))))

(defn game-print [cmd]
  (println cmd))

(def allowed-commands #{'look 'walk 'pickup 'inventory})

(defn game-eval [sexp]
  (if (allowed-commands (first sexp))
    (eval sexp)
    "I don't know that command"))

(defn game-repl []
  (loop [cmd (game-read)]
    (if (= 'quit (first cmd))
      true
      (do
        (game-print (game-eval cmd))
        (recur (game-read))))))
