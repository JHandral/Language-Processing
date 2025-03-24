; 1)
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind how long precisely having little or no money in my purse , and nothing particular to interest me on shore , I thought I would sail about a little and see the part of the world . It is a way I have of driving off the spleen , and regulating the circulation . Whenever I find myself growing grim about the mouth whenever it is a damp , drizzly November in my soul whenever I find myself involuntarily pausing before coffin warehouses , and bringing up the rear of every funeral I meet and especially whenever my hypos get such an upper hand of me , that it requires a strong principle to prevent me from deliberately stepping into the street , and methodically knocking people's hats off then , I account it high time to get to sea as soon as I can . This is my substitute for pistol and ball . With a flourish Cato throws himself upon his sword I quietly take to the ship . There is nothing surprising in this . If they but knew it , almost all men in their degree , some time or other , cherish very nearly the same feelings toward the ocean with me .)) 

(defn member-of-list? [w l] (if (empty? l) false (if (= w (first l)) true (member-of-list? w (rest l))))) 

(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
    vocab
    (if (member-of-list? (first word-tokens) vocab)
      (get-vocabulary (rest word-tokens) vocab)
      (get-vocabulary (rest word-tokens) (conj vocab (first word-tokens))))))

(def moby-vocab (get-vocabulary moby-word-tokens '()))
; 2)
(def word (list 'the 'the 'man))

(defn get-count-of-word [w word-tokens count]
  (if (empty? word-tokens)
    count
    (if (= w (first word-tokens))
      (get-count-of-word w (rest word-tokens) (+ 1 count))
      (get-count-of-word w (rest word-tokens) count))))

(defn get-word-counts [vocab word-tokens] (let [count-word (fn [w] (get-count-of-word w word-tokens 0))] (map count-word vocab)))

; 3)
(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))


; 4)
(defn flip [p] (if (< (rand 1) p) true false)) 

(defn normalize [params] (let [sum (apply + params)] (map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params] (if (flip (first params)) (first outcomes) (sample-categorical (rest outcomes) (normalize (rest params))))) 

(defn create-uniform-distribution [outcomes] (let [num-outcomes (count outcomes)] (map (fn [x] (/ 1 num-outcomes)) outcomes)))

(defn sample-uniform-BOW-sentence [n vocab]
  (let [distribution (create-uniform-distribution vocab)]
  (repeatedly n #(sample-categorical vocab distribution)))
  )


;5)

(defn compute-uniform-BOW-prob [vocab sentence] (let [x (first (create-uniform-distribution vocab))] (apply * (repeat (count sentence) x))))

(def vocab  (list 'the 'a 'every))
(def v (list 'every 'every))
;(println (compute-uniform-BOW-prob vocab v))

;6
;(def sample1 
(print sample-uniform-BOW-sentence 3 moby-vocab)
(print (compute-uniform-BOW-prob moby-vocab (sample-uniform-BOW-sentence 3 moby-vocab)))
;(def sample2 (sample-uniform-BOW-sentence 3 moby-vocab))
;(def sample3 (sample-uniform-BOW-sentence 3 moby-vocab))
;(print (sample1))
; the probability is always going to stay the same since we are using a uniform dist

;7
(def moby-word-probabilities (normalize moby-word-frequencies))

;8
(defn sample-BOW-sentence [len vocabulary probabilities]
(if (= len 0)
'()

(cons (sample-categorical vocabulary probabilities)
(sample-BOW-sentence (- len 1) vocabulary probabilities))))

;(println (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
;(print sample-BOW-sentence 3 moby-vocab moby-word-probabilities)
;(print sample-BOW-sentence 3 moby-vocab moby-word-probabilities)

(let [sample-sentence (sample-BOW-sentence 3 moby-vocab moby-word-probabilities)]
(println sample-sentence))

; sail way the
; money shore no
; mind a to

;9
(defn lookup-probability [w outcomes probs]
  (if (= (first outcomes) w)
   (first probs)
   (lookup-probability w (rest outcomes) (rest probs))))

;10
(defn product [l]
(apply * l))

(defn compute-BOW-prob [sentence vocabulary probabilities]
  (let [probs (map (fn [word] (lookup-probability word vocabulary probabilities)) sentence)]
    (product probs)))

;11
(let [samp (sample-BOW-sentence 3 moby-vocab moby-word-probabilities)]
  (print (compute-BOW-prob samp moby-vocab moby-word-frequencies)))

; the probability should be higher because there is not an even distribution
;  the probabilities are different, because in a uniform BOW model, every word has the same probability, whereas in the Moby Dick corpus BOW model, the probabilities are based on the relative frequencies of the words in the corpus
; you can construct different sentences with the same probability according to the BOW model as long as they consist of the same words (order doesn't matter)
;when computing the probability of a sentence under a BOW model, the only necessary information is the list of words in the sentence and the probability of each word in the vocabulary. 