(ns markov-elear.generator
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

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

(defn generate-text [start-phrase word-chain]
  (let [prefix (s/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)
        result-text (word-chain->text result-chain)]
    result-text))

(defn file->word-chain [path]
  (text->word-chain (slurp (io/resource path))))

(def files ["poem.txt" "monad.txt" "clojure.txt" "intro.txt" "unix.txt"
            "functional_programming.txt" "edward_lear.txt" "computer_science.txt"])

(def functional (apply merge-with clojure.set/union (map file->word-chain files)))

(def prefix-list ["On the" "They went" "And all" "For every"
                  "To a" "And every" "For his" "And the"
                  "But the" "Are the" "The Pobble" "For the"
                  "When we" "In the" "Yet we" "With only"
                  "Are the" "Though the" "And when" "And this"
                  "With a" "And at" "What a" "Of the"
                  "O please" "So that" "And all" "When they"
                  "And nobody" "And it's" "For example" "Also in"
                  "In contrast" "The history" "The examples" "Can you"
                  "I am" "Where did" "The theory" "I expected him"
                  "Lets take" "It was" "This is" "You are"
                  "What are"])

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (s/replace result-text #"[,| ]$" ".")]
    (s/replace cleaned-text #"\"" "'")))

(defn empty-sentence? [sentence]
  (<= (count (s/split sentence #" ")) 2))

(defn tweet-text [chain]
  (let [text (generate-text (-> prefix-list shuffle first) chain)]
    (end-at-last-punctuation text)))

(defn functional-tweet-text []
  (first (drop-while empty-sentence? (repeatedly #(tweet-text functional)))))
