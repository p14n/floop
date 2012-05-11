(ns floop.core)
(use '[clojure.contrib.seq :only (positions)])

(defn buffered-token-reader
  "Buffers an input stream, splitting by the provided token. Returns a lazy sequence of byte sequences."
  [input token]
  (let [bytes (byte-array 1024) read-token (fn this [previous]
   (.read input bytes)
    (let [combined (concat previous bytes)
          token-index (index-of-seq-in-seq combined token)
          end-token-index (+ token-index (count token))]
     (if
      (> token-index -1)
       (cons
        (take end-token-index combined)
        (lazy-seq (this (drop end-token-index combined))))
       (lazy-seq (this combined)))))]
      (read-token (byte-array 0))))

(defn index-of-seq-in-seq
  "Returns the index of one sequence within another"
  [major minor]
  (let [groups (partition (count minor) 1 major)
        minorpositions (positions #{(seq minor)} groups)]
   (if
    (> (count minorpositions) 0)
    (first minorpositions)
    -1)))
