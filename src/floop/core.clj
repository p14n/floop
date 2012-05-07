(ns floop.core
 (:import
  (java.util.regex Pattern)
  (java.io PrintWriter BufferedReader)))
(use '[clojure.contrib.server-socket :only (create-server close-server)])
(use '[clojure.contrib.duck-streams :only (write-lines read-lines writer reader)])
(use '[clojure.string :only (split blank?)])

(defn toOut [input output]
  (with-open [#^PrintWriter writer (writer (java.io.PrintWriter. output true))]
   (loop [input input]
   (when-let [inputCopy (first input)]
    (println inputCopy)
    (.write writer (str inputCopy))
    (.println writer)
    (recur (rest input))))))

(defn echo [input output]
  (toOut (read-lines input) output))

(defn split-input [divide]
  (fn [input]
    (split input (Pattern/compile divide))))

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

;   (lazy-seq
;    (if-let [line (.readLine rdr)]
;      (line-to-sequence line termination)
;      (.close rdr))))]
;    (read-line (reader input))))
                                        ;return head + call read line with tail
      ;otherwise just return read-line with line
    
                                        ;wait for a new value.  if the new value is a termination,
;add to the sequence head

;      (let [index (.indexOf line termination) term-length (.length termination)]
;       (if
;        (> index -1)
;         (cons (subs line 0 index) (cons (subs line (+ index term-length)) (this rdr) ))
;         (cons line (this rdr)))


(defn echo-with-termination [termination]
  (fn [input output]
    (write-lines output (read-lines-with-termination input termination))))

; if the returned line matches termination, return the prefix,suffix and cons the rest
; if not, just return the line and cons the rest
;  (let [read-line (fn this [#^BufferedReader rdr]
;                    (lazy-seq
;                     (if-let [line (.readLine rdr)]
;                       (cons line (this rdr))
;                       (.close rdr))))]
;    (read-line (reader f))))
; 
                                        ;(create-server 12222 echo)