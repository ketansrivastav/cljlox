(ns celjlox.lexer)

(def allowed-tokens ["!" "="])
(def allowed-tokens-2-chars ["=="])

(defn get-until
  [predicate source position]
  (loop [current position lexeme ""]
    (if (and (<= (inc current) (count source)) (predicate (str (nth source current))))
      (recur (inc current) (str lexeme (str (nth source current))))
      lexeme)))

(defn lex->token
  [token position]
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
         ;; check for 2 length char 
         (if (and (< (inc character-pointer) (count source)) (some #(= % (str ch (str (nth source (inc character-pointer))))) allowed-tokens-2-chars))
           (recur (+ character-pointer 2) [(conj tokens (lex->token (str ch (nth source (inc character-pointer))) character-pointer)) errors])

           (do
             ;; check for 1 length char 
             (if (some #(= % ch) allowed-tokens)

               (recur (+ character-pointer (count ch)) [(conj tokens (lex->token ch character-pointer)) errors])
               (do
                 ;; check for literals : numbers     
                 (if (digit? ch)
                   (let [literal (get-until digit? source character-pointer)]
                     (recur (+ character-pointer (count literal)) [(conj tokens (lex->token literal character-pointer)) errors]))
                   ;; no matchinng clause
                   (recur (inc character-pointer) [tokens errors])))))))))
   0 [[] []]))
(defn- digit? [ch]
  (not (nil? (re-matches #"\d" ch))))
(scan "hell=903 open==0hel=o3")
