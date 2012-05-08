(ns floop.test.core
  (:use [floop.core])
  (:use [clojure.test]))

(deftest should-return-sequence-containing-original-text
  (is (= (line-to-sequence "simple" "xxx") ["simple"])))

(deftest should-return-sequence-of-original-split-by-xxx
  (is (= (line-to-sequence "morexxxcomplex" "xxx") ["morexxx" "complex"])))

(deftest should-return-original-from-buffer
  (is (= (read-lines-with-termination (java.io.StringReader. "simplexxx") "xxx") ["simplexxx"])))

(deftest should-return-split-from-buffer
  (is
   (=
    (read-lines-with-termination
      (java.io.StringReader. "morexxxcomplexxx") "xxx")
    ["morexxx" "complexxx"])))

(deftest should-return-split-from-multiple-line-buffer
  (is
   (=
    (read-lines-with-termination
      (java.io.StringReader. "more\nxxxcomplexxx") "xxx")
    ["morexxx" "complexxx"])))
