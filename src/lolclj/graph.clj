(ns lolclj.graph
  ; I don't like importing this as "string" or "str" as that shadows some common names
  (:require [clojure.string])
  (:require [clojure.java.shell :as shell]))

; Chapter 7, graph generation

(def nodes
  {:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
   :garden      "You are in a beautiful garden. There is a well in front of you."
   :attic       "You are in the attic. There is a giant welding torch in the corner."})

(def edges
  {:living-room [[:garden :west :door], [:attic :upstairs :ladder]]
   :garden      [[:living-room :east :door]]
   :attic       [[:living-room :downstairs :ladder]]})

(defn substitute-if [val pred coll]
  (for [i coll] (if (pred i) val i)))

; matches LoL, but slow and more complex
(defn dot-name-slow [exp]
  (apply str (substitute-if \_ (complement #(Character/isLetterOrDigit %)) (str exp))))

; 100x faster:
(defn dot-name [exp]
  (clojure.string/replace exp #"\W" "_"))

(def max-label-length 30)

(defn dot-label [exp]
  (if exp
    (let [s (print-str exp)]
      (if (> (.length s) max-label-length)
        (str (subs s 0 (- max-label-length 3)) "...")
        s))
    ""))

(defn keyword-to-str [k] (subs (str k) 1))

(defn nodes->dot [nodes]
  (doseq [[node description] nodes]
    (let [node-name (keyword-to-str node)]
      (print (dot-name node-name))
      (print "[label=\"")
      (print (dot-label (str (clojure.string/upper-case node-name) " " description)))
      (println "\"];"))))

(defn edges->dot [edges]
  (doseq [[from-node edge-list] edges]
    (doseq [[to-node direction via] edge-list]
      (print (dot-name (keyword-to-str from-node)))
      (print "->")
      (print (dot-name (keyword-to-str to-node)))
      (print "[label=\"")
      (print (keyword-to-str direction) (keyword-to-str via))
      (println "\"];"))))

(defn maplist [f coll]
  (for [i (iterate next (seq coll)) :while i] (f i)))

; this is too complex... the (let ...) builds uedge-map, a map of { [node-a
; node-b] : [edge] } first by building a seq of [ [k v] .. ] where k is a
; node-pair, and v is an edge. Node pairs [node-a node-b] are sorted so that
; directed edges from a->b and b<-a will map to the same key. The seq is then
; zipmap'ed up into the uedge-map.
(defn uedges->dot [edges]
  (let [kvseq (mapcat (fn [[from-node edge-list]]
                        (map (fn [[to-node direction via]]
                               [(sort [from-node to-node]) direction via])
                             edge-list))
                      edges)
        uedge-map (zipmap (map first kvseq) (map next kvseq))]
    (doseq [[[from-node to-node] [direction via]] uedge-map]
      (print (dot-name (keyword-to-str from-node)))
      (print "--")
      (print (dot-name (keyword-to-str to-node)))
      (print "[label=\"")
      (print (keyword-to-str direction) (keyword-to-str via))
      (println "\"];"))))

(defn graph->dot [nodes edges]
  (println "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (println "}"))

(defn ugraph->dot [nodes edges]
  (println "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (println "}"))

(defn dot->png [fname thunk]
  (spit fname (with-out-str (thunk)))
  (shell/sh "/usr/local/bin/dot" "-Tpng" "-O" fname))

(defn graph->png [fname nodes edges]
  (dot->png fname #(graph->dot nodes edges)))

(defn ugraph->png [fname nodes edges]
  (dot->png fname #(ugraph->dot nodes edges)))

