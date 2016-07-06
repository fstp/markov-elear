(ns markov-elear.nlp
  (:require [clojure.pprint :refer [pprint]]
            [opennlp.nlp :as n]
            [clojure.java.io :as io]))

(defn respath "Helper to get the full path of a resource given its name." [name] (str (io/resource name)))

;; Create NLP functions.
(def tokenize (n/make-tokenizer (respath "models/en-token.bin")))
(def get-sentences (n/make-sentence-detector (respath "models/en-sent.bin")))
(def detokenize (n/make-detokenizer (respath "models/english-detokenizer.xml")))
(def pos-tag (n/make-pos-tagger (respath "models/en-pos-maxent.bin"))) 
