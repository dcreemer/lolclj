(ns lolclj.wumpus
  (:use lolclj.graph)
  (:require clojure.set))

; Chapter 8, Grand Theft Wumpus

; global variables
(def congestion-city-nodes {})
(def congestion-city-edges {})
(def visited-nodes [])

(def node-num 30)
(def edge-num 45)
(def worm-num 3)
(def cop-odds 15)

; nodes and edges

(defn random-node []
  (inc (rand-int node-num)))

(defn edge-pair [a b]
  (when-not (= a b)
    [[a b] [b a]]))

(defn make-edge-list []
  (apply concat
         (repeatedly edge-num #(edge-pair (random-node) (random-node)))))

(defn direct-edges
  "find all the edges in an edge list that start from the given node"
  [node edge-list]
  (filter #(= (first %) node) edge-list))

; adapted from clojure.contrib.graph. Builds up the list of nodes
; to check as it builds up the list of nodes visited.
(defn get-connected
  "return a lazy seq of all nodes reachable from a source node"
  [node edge-list]
  (letfn [(traverse [nodes-to-visit visited]
            (lazy-seq
             (let [s (seq (drop-while visited nodes-to-visit))
                   n (first s)
                   ns (rest s)]
               (when s
                 ; uncomment this to see the shape of the recursion
                 ; (println s)
                 (cons n (traverse (concat (map last (direct-edges n edge-list)) ns)
                                   (conj visited n)))))))]
    (traverse [node] #{})))

(defn find-islands
  "return a (lazy) seq of each 'island' of connected nodes e.g. ((1 2) (3...30))"
  [nodes edge-list]
  (letfn [(find-island [nodes]
            (lazy-seq
             (let [connected (set (get-connected (first nodes) edge-list))
                   unconnected (seq (clojure.set/difference (set nodes) connected))]
               (cons (seq connected)
                     (if unconnected
                       (find-island unconnected))))))]
    (find-island nodes)))

(defn connect-with-bridges
  "return a sec of the edges needed to connect the islands"
  [islands]
  (when (next islands)
    (concat (edge-pair (ffirst islands) (ffirst (next islands)))
            (connect-with-bridges (next islands)))))

(defn connect-all-islands
  "return an updated edge-list with all islands connected"
  [nodes edge-list]
  (concat (connect-with-bridges (find-islands nodes edge-list)) edge-list))