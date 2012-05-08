(ns floop.core
 (:import
  (java.util.regex Pattern)
  (java.io PrintWriter BufferedReader)))
(use '[clojure.contrib.server-socket :only (create-server close-server)])
(use '[clojure.contrib.duck-streams :only (write-lines read-lines writer reader)])
(use '[clojure.string :only (split blank?)])

(defn line-to-sequence
  "Converts a string into a sequence, divided by the termination string.
   The termination string is included at the end of the preceding token."
  [line termination]
  (let [index (.indexOf line termination)
        term-length (.length termination)
        term-end (+ index term-length)]
  (if
   (> index -1)
    (cons
     (subs line 0 term-end)
     (seq
      [(subs line term-end)]))
    (seq [line]))))

(defn read-lines-with-termination
  "Buffers an input stream, splits the input by the termination pattern
   and returns as a lazy sequence"
  [input termination]
  (let [read-line-with-previous (fn this [#^BufferedReader rdr previous]
   (let [line (str previous (.readLine rdr))]
    (println (str "line " line " prev " previous))
    (if (blank? line)
     (.close rdr)
     (if (> (.indexOf line termination) -1)
       (let [splitline (line-to-sequence line termination)]
         (println splitline)
         (lazy-seq
          (cons (first splitline) (this rdr (second splitline)))))
      (this rdr line)))))]
    (read-line-with-previous (reader input) "")))