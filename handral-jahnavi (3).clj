;1)
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))
(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (/ 1 0)
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
       (list-foldr f base (rest lst)))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr
    (fn [word rest-score]
      (+ (log2 (score-categorical word vocabulary probabilities))
         rest-score))
    0
    sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
    (fn [sen rst]
      (+ (score-BOW-sentence sen probabilities) rst))
    0
    corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
       (log2
         (apply +
           (map (fn [z] (Math/pow 2 z))
             (map (fn [x] (- x mx)) log-vals)))))))

;(defn theta-corpus-joint [theta corpus theta-probs]
 ; (let [corpus-prob (score-corpus corpus theta)
  ;      theta-prior-prob (log2 (score-categorical theta thetas theta-probs))]
   ; (+ corpus-prob theta-prior-prob)))

(defn theta-corpus-joint [theta corpus theta-probs]
  (if (= theta (first thetas))
    (+ (score-corpus corpus theta)(log2 (first theta-probs)))
    (+ (score-corpus corpus theta)(log2 (first (rest theta-probs))))))
(println (theta-corpus-joint theta1 my-corpus theta-prior))

(theta-corpus-joint theta1 my-corpus theta-prior)


;2)
(defn compute-marginal [corpus theta-probs]
  (let [log-joint-probs (map (fn [theta] (theta-corpus-joint theta corpus theta-probs)) thetas)]
    (logsumexp log-joint-probs)))

(def my-corpus '((call me)
                 (call ishmael)))

(compute-marginal my-corpus theta-prior)

;-6.415037499278844

;3)
(defn compute-conditional-prob [theta corpus theta-probs]
  (let [joint-prob (theta-corpus-joint theta corpus theta-probs)
        marginal-prob (compute-marginal corpus theta-probs)]
    (- joint-prob marginal-prob)))

;4)
(defn compute-conditional-dist [corpus theta-probs]
  (map (fn [theta] (compute-conditional-prob theta corpus theta-probs)) thetas))

;5) idk come back
(defn exponentiate [lst]
  (if (empty? lst)
    '()
    (cons (Math/pow 2 (first lst)) (exponentiate (rest lst)))))
(println (exponentiate (compute-conditional-dist my-corpus theta-prior)))
; I think the distribution looks like this because of the size of teh corpus (3 words)

;6)
;it computes the probability of observing the same my-corpus again given the updated beliefs about θ after having observed my-corpus once. If you compare this with the marginal likelihood computed before updating the beliefs about θ, this quantity should be higher. This is because the beliefs about θ have been updated to better fit my-corpus, so the probability of observing my-corpus again should be higher under these updated beliefs.
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist (compute-conditional-dist observed-corpus theta-probs)]
    (compute-marginal new-corpus conditional-dist)))


;7)
(defn normalize [params]
(let [sum (apply + params)]
(map (fn [x] (/ x sum)) params)))
(defn flip [weight]
(if (< (rand 1) weight)
true
false))
(defn sample-categorical [outcomes params]
(if (flip (first params))
(first outcomes)
(sample-categorical (rest outcomes)
(normalize (rest params)))))
(defn repeat [f n]
(if (= n 0)
'()
(cons (f) (repeat f (- n 1)))))
(defn sample-BOW-sentence [len probabilities]
(if (= len 0)
'()
(cons (sample-categorical vocabulary probabilities)
(sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
  (repeat (fn [] (sample-BOW-sentence sent-len theta)) corpus-len))

;8)
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta (sample-categorical thetas theta-probs)
        corpus (sample-BOW-corpus theta sent-len corpus-len)]
    (list theta corpus)))

;9)
(defn get-theta [theta-corpus]
(first theta-corpus))
(defn get-corpus [theta-corpus]
(first (rest theta-corpus)))
(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
(repeat (fn [] (sample-theta-corpus sent-len corpus-len theta-probs)) sample-size))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
  (let [sampled-corpora (map get-corpus (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))
        occurrence-count (count (filter #(= % corpus) sampled-corpora))]
    (/ occurrence-count sample-size)))

;10)
(estimate-corpus-marginal my-corpus 50 2 2 theta-prior)
(estimate-corpus-marginal my-corpus 10000 2 2 theta-prior)

; the first call has larger likelihoods than the second call usually

;11)
(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
  (let [theta-corpus-pairs (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)
        matching-pairs (reduce (fn [acc pair]
                                (if (= observed-corpus (get-corpus pair))
                                  (conj acc pair)
                                  acc))
                              []
                              theta-corpus-pairs)
        count-theta (get-count theta (map get-theta matching-pairs) 0)
        remaining-pairs (count matching-pairs)]
    (if (zero? remaining-pairs)
      0
      (/ count-theta (float remaining-pairs)))))



;12)
; the results varied a lot. a sample size of about 10000 gave stable results, because it was a 
; larger and better representation of the distribution




