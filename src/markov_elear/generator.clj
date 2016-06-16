(ns markov-elear.generator)

(defn chain-entry [words]
  (let [[a b c] words]
    {[a b] (if c #{c} #{})}))

(defn word-chain
  [word-transitions]
  (reduce (fn [result words]
            (merge-with clojure.set/union
                        result
                        (chain-entry words)))
          {}
          word-transitions))

(defn text->word-chain
  "Construct a word chain from text"
  [s]
  (let [words (clojure.string/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn word-chain->text
  "Convert word chain to text"
  [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (word-chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140) ;; 140 is the character limit for Tweets!
          result
          (recur new-prefix chain (conj result suffix)))))))
