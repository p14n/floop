(ns floop.test.core
  (:use [floop.core])
  (:use [clojure.test]))

(deftest should-return-two-buffered-tokens
  (let [input (java.io.ByteArrayInputStream. (.getBytes "notxxxcomplexxx"))
        readseq (buffered-token-reader input (.getBytes "xxx"))]
    (is
     (= (first readseq) (seq (.getBytes "notxxx"))))
    (is
     (= (second readseq) (seq (.getBytes "complexxx"))))))

(deftest should-find-index-of-bytes
  (is
   (= 3 (index-of-seq-in-seq (.getBytes "mybigsequence") (.getBytes "igse")))))

(deftest should-find-one-recording-dataset
  (is
   (= 1 (count (get-recording-datasets [
                                        {:name "hi" :recording true}
                                        {:name "lo" :recording false}])))))