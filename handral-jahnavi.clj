; 1)
(defn sequence-to-power [x n]
  (if (= 1 n)
    x
    (let [powered-seq (sequence-to-power x (- n 1))]
      (concat x powered-seq))))

; 2)
(defn in-L? [x]
  (if (empty? x)
    true
    (if (= (first x) 'b)
      (in-L? (rest x))
      false)))

; 3)
(defn add-to-end [l x]
  (if (empty? l)
    (list x)
   (cons (first l) (add-to-end (rest l) x))))
(defn rep-char [c n]
  (if (zero? n)
    '()
    (cons ‘c (rep-char c (- n 1)))))
(defn generate-bn-an[k]
  (add-to-end (rep-char k 'a) (rep-char k 'b)))



; 4)
(defn reverse-helper [lst rev-lst]
  (if (empty? lst)
    rev-lst
    (reverse-helper (rest lst) (cons (first lst) rev-lst))))

(defn reverse [lst]
  (reverse-helper lst '()))

(defn remove-last-element[l]
  (reverse (rest (reverse l))))


; 5)
(defn reverse-helper [lst rev-lst]
  (if (empty? lst)
    rev-lst
    (reverse-helper (rest lst) (cons (first lst) rev-lst))))

(defn reverse [lst]
  (reverse-helper lst '()))


(defn remove-last-element[l]
  (reverse (rest (reverse l))))


(defn recognize-bn-an [str]
  (if (and (= (first str) 'b) (= (last str) 'a))
    true
    (if (or (= (first str) 'b) (= (last str) 'a))
      false
      (recognize-bn-an (remove-last-element (remove-last-element str))))))


;6)
(defn concat-L-A [L A]
  (map (fn [x] (concat L x)) A))

; 7)
; Let A = {b} and B = {bb}. Then, concat(A, B) = {bbb} and concat(B, A) = {bbb}.

;8)
; Let A = {aa} and B = {b}. Then, concat(A, B) = {aab} and concat(B, A) = {baa}

;9) L = {E}, language containing the empty string
