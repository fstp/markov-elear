(ns markov-elear.nlp
  (:require [opennlp.nlp :as n]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn respath "Helper to get the full path of a resource given its name." [name] (str (io/resource name)))

;; Create NLP functions.
(def tokenize (n/make-tokenizer (respath "models/en-token.bin")))
(def get-sentences (n/make-sentence-detector (respath "models/en-sent.bin")))
(def detokenize (n/make-detokenizer (respath "models/english-detokenizer.xml")))
(def pos-tag (n/make-pos-tagger (respath "models/en-pos-maxent.bin"))) 

(def files [(io/resource "monad.txt")
            (io/resource "clojure.txt")
            (io/resource "unix.txt")])

(defn tokenize-and-tag [file]
  (->> (slurp file)
       (tokenize)
       (pos-tag)))

(defn merge-storage [x y]
  (merge-with set/union x y))

(defn create-storage [file]
  (reduce (fn [result [word tag]]
            (merge-with set/union result {tag #{word}}))
          {}
          (tokenize-and-tag file)))

(def storage
  (->> (map create-storage files)
       (reduce merge-storage)
       (clojure.walk/keywordize-keys)))

(defn tag->word [tag]
  (first (shuffle (tag storage))))

;;[:NNP :VBZ :NN :IN :DT]

