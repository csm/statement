(ns stmt.apple-card
  (:require [clojure.java.shell :as sh]
            [clojure.string :as string]))

(def transactions-start #"Transactions")
(def transactions-end #"(Page [0-9]+ ?/[0-9]+)|(Total Daily Cash this month)")

(defn append-last
  [seqs item]
  (conj (vec (butlast seqs))
        (conj (vec (last seqs)) item)))

(defn seqs-between
  [input begin end]
  (loop [seqs []
         reading-seq false
         input input]
    (if (empty? input)
      seqs
      (let [[next-seqs next-reading] (if reading-seq
                                       (if (re-matches end (first input))
                                         [(conj (vec seqs) []) false]
                                         [(append-last seqs (first input)) true])
                                       (if (re-matches begin (first input))
                                         [seqs true]
                                         [seqs false]))]
        (recur next-seqs next-reading (rest input))))))

(defn seq->map
  [seq keys]
  (when (>= (count seq) (count keys))
    (let [item (apply hash-map (interleave keys (take (count keys) seq)))
          rest (seq->map (drop (count keys) seq) keys)]
      (if rest
        (conj rest item)
        (list item)))))

(defn seqs->maps
  [seqs keys]
  (apply concat (map #(seq->map % keys) seqs)))

(defn parse-apple-card-statement
  [file]
  (let [result (sh/sh "pdftotext" file "-")]
    (if (not (zero? (:exit result)))
      {:cognitect.anomalies/category :fault
       :message (:err result)}
      (let [lines (->> (:out result)
                       (re-seq #"([^\r\n]*)(\r?\n)")
                       (map second)
                       (map string/trim)
                       (remove empty?))
            transactions (map #(drop 4 %) (seqs-between lines transactions-start transactions-end))]
        {:transactions (seqs->maps transactions [:date :description :daily-cash-percent :daily-cash :amount])}))))
