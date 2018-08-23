(ns spell-checker.core
(:require  [clojure.string  :as str ] )
(:use clojure.core.matrix )
)


(defn read-file [ path ]
  "This function reads and returns the content of a file with a given path"
  (slurp path)
 )


(defn to-words [ wordstring  ]
  "Convert a given sequence of newline delimetered string into a set of words"
 (set  (map  #(str/trim %)   (str/split-lines  wordstring) ) )
)

;; Our dictionary
(def words  (-> "/usr/share/dict/words"
                (read-file)
                (to-words)
             ))



(defn correct-word? [word ]
  "Check if a word is in our dictionary"
 (contains? words word)
)



;; An Implementation of the damerau–levenshtein inspired by
;;https://stackoverflow.com/users/3687048/mark

(defn damerau–levenshtein [fstr sstr]
  (let [fstr-len (count fstr)
        sstr-len (count sstr)
        mtx (new-matrix :ndarray (inc fstr-len) (inc sstr-len))]
   (mset! mtx 0 0 0)
   (dotimes [i fstr-len]
     (mset! mtx (inc i) 0 (inc i)))
   (dotimes [j sstr-len]
     (mset! mtx 0 (inc j) (inc j)))
   (dotimes [i fstr-len]
     (dotimes [j sstr-len]
       (let [i+ (inc i) j+ (inc j)
             i- (dec i) j- (dec j)
             cost (if (= (.charAt fstr i)
                         (.charAt sstr j))
                    0 1)]
         (mset! mtx i+ j+
                (min (inc (mget mtx i j+))
                     (inc (mget mtx i+ j))
                     (+ (mget mtx i j) cost)))
         (if (and (pos? i) (pos? j)
                  (= (.charAt fstr i)
                     (.charAt sstr j-))
                  (= (.charAt fstr i-)
                     (.charAt sstr j)))
           (mset! mtx i+ j+
                  (min (mget mtx i+ j+)
                       (+ (mget mtx i- j-) cost)))))))
   (mget mtx fstr-len sstr-len)))




(defn minimun-distance [ word ]
  ( apply min-key (partial damerau–levenshtein word) words ) )




(defn -main  [& args]
  (time
    ( let [ _ (println "Enter word possibly misspelled :" )
            word  (read-line) ]
    ( if ( correct-word? word)
        (println word  "Is a correct word" )

        (do
          (println word "Is wrong word ")
          (println "Did you mean -> " (minimun-distance word)  )
        )
    )
   )
))
