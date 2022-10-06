(ns stmt
  "Parse credit card PDF statements."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as string]))

(def apple-transactions-start #"Transactions")
(def apple-transactions-end #"(Page [0-9]+ ?/[0-9]+)|(Total Daily Cash this month)")

(def capitalone-start #"([A-Z ]+ #[0-9]+: Transactions)|(Transactions .Continued.)")
(def capitalone-end #"(Additional Information on the next page)|([A-Z ]+ #[0-9]+: Total Transactions)")

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

(defn parse-statement
  [file begin end keys drop-header-count]
  (let [result (sh/sh "pdftotext" file "-")]
    (if (not (zero? (:exit result)))
      {:cognitect.anomalies/category :fault
       :cognitect.anomalies/message (:err result)}
      (let [lines (->> (:out result)
                       (re-seq #"([^\r\n]*)(\r?\n)")
                       (map second)
                       (map string/trim)
                       (remove empty?))
            transactions (map #(drop drop-header-count %) (seqs-between lines begin end))]
        {:transactions (seqs->maps transactions keys)}))))

(defn parse-apple-card-statement
  "Parses an Apple Card PDF statement into a map.

  Result map will contain:

  - :transactions, a seq of maps containing keys:
    - :date
    - :description
    - :daily-cash-percent
    - :daily-cash
    - :amount

  Returns an anomaly map on error."
  [file]
  (parse-statement file apple-transactions-start apple-transactions-end
                   [:date :description :daily-cash-percent :daily-cash :amount]
                   4))

(defn parse-capital-one-statement
  "Parses an Apple Card PDF statement into a map.

  Result map will contain:

  - :transactions, a seq of maps containing keys:
    - :trans-date
    - :post-date
    - :description
    - :amount

  Returns an anomaly map on error."
  [file]
  (parse-statement file capitalone-start capitalone-end [:trans-date :post-date :description :amount] 4))