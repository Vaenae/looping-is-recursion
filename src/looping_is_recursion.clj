(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [result base exp]
        (cond (zero? exp) result
              (= 1 exp) (* base result)
              :else (recur (* base result)
                           base
                           (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (>= 1 (count a-seq)) (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (not= (count seq1) (count seq2)) false
        (and (empty? seq1) (empty? seq2)) true
        (= (first seq1) (first seq2))
        (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0 a-seq a-seq]
  (cond (empty? a-seq) nil
        (pred (first a-seq)) index
        :else (recur (inc index) (rest a-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq) 0
  (loop [sum 0 items 0 a-seq a-seq]
    (if (empty? a-seq) (/ sum items)
      (recur (+ (first a-seq) sum)
             (inc items)
             (rest a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [results #{} a-seq a-seq]
    (if (empty? a-seq) results
      (recur (toggle results (first a-seq))
             (rest a-seq)))))

(defn fast-fibo [n]
  (cond (>= 0 n) 0
          (= 1 n) 1
          :else
        (loop [n n res1 1 res2 0]
        (if (> 2 n) res1
          (recur (dec n) (+ res1 res2) res1)))))

(defn cut-at-repetition [a-seq]
  (loop [result [] a-seq a-seq]
    (cond (empty? a-seq) result
          (contains? (set result) (first a-seq))
          result
          :else (recur (conj result (first a-seq))
                       (rest a-seq)))))

