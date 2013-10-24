(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                    acc
                    (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)
    ))

(defn last-element [a-seq]
  (cond
   (empty? a-seq) nil
   (== (count a-seq) 1) (first a-seq)
   :else (recur (rest a-seq))
   ))

(defn seq= [seq1 seq2]
  (loop [a-seq seq1
         b-seq seq2]
    (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (empty? a-seq) false
     (empty? b-seq) false
     (not (== (first a-seq) (first b-seq))) false
     :else (recur (rest a-seq) (rest b-seq)))))



(defn find-first-index [pred a-seq]
  (loop [seq a-seq
         pos 0]
    (cond
     (empty? seq) nil
     (pred (first seq)) pos
     :else (recur (rest seq) (inc pos)))))

(defn avg [a-seq]
  (loop [pos 0
         avg 0]
    (if (nil? (get a-seq pos))
      (/ avg pos)
      (recur (inc pos) (+ avg (get a-seq pos))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))


(defn parity [a-seq]
  (loop [my-set #{}
         seq a-seq]
    (if (empty? seq)
      my-set
      (recur (toggle my-set (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [f2 0
         f1 1
         val n]
    (if (zero? val)
      f2
      (recur f1 (+ f2 f1) (dec val)))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         new-seq []]
    (if (or (empty? seq) (contains? (set new-seq) (first seq)))
      (reverse new-seq)
      (recur (rest seq)
             (cons (first seq) new-seq)))))

