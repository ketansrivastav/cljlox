(ns celjlox.lexer)

(def allowed-tokens ["!" "="])
(def allowed-tokens-2-chars ["!!" "==" "="])

(defn get-until
  [predicate source position]
  (loop [current position lexeme ""]
    (if (and (<= (inc current) (count source)) (predicate (nth source current)))
      (recur (inc current) (str lexeme (str (nth source current))))
      lexeme)))

(defn lex->token [token position]
  {:token token
   :postiion position})

(defn scan
  [source]
  ((fn scan*
     [character-pointer [tokens errors]]
     "recursivly lex one character -> token at a time"
     (if (>= character-pointer (count source))
       [tokens errors]
       (let [ch (str (nth source character-pointer))]
         ;; check for 1 char 
         (if (some #(= % ch) allowed-tokens)
           (recur (+ character-pointer (count ch)) [(conj tokens lex->token) errors])

           (if (some #(= % ch) allowed-tokens-2-chars)
             (recur (+ character-pointer (count ch)) [(conj tokens lex->token) errors])

             (recur (inc character-pointer) [tokens errors]))))))
   0 [[] []]))

#_(scan "hell=o
            open ==0
            hel =o ")
(get-until (fn [ch]
             (print "->" ch)
             (-> ch
                 (str)

                 (read-string)
                 (number?))) "0011288=3" 0)

