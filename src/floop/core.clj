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

(defn record [msg datasets])

(defn feed-and-record
  "Takes an input and output, and records the traffic between the two"
  [input output datasets token]
  (let [recording (get-recording-datasets datasets)
        record-and-feed (fn this [msg] (
                           (if (> (count recording) 0)
                            (record msg recording))
                           (.write output (byte-array msg))))]
        (doseq [message (buffered-token-reader input token)]
          (record-and-feed message))))

(defn get-recording-datasets
  "Returns a sequence of datasets in the recording state"
  [datasets]
  (filter (fn [x] (x :recording)) datasets))
                      