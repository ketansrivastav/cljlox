(ns celjlox.lexer)

(def allowed-tokens [">" "<" "=" "+" "-"])
(def allowed-tokens-2-chars ["==" "!=" "<=" ">="])

(defn- digit? [ch]
  (not (nil? (re-matches #"\d" ch))))

(defn- identifier-characters? [ch]
  (not (nil? (re-matches #"[a-z]|_" ch))))

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
       (let [ch (str (nth source character-pointer))
             can-look-ahead? (< (inc character-pointer) (count source))
             ch2 (if can-look-ahead? (str ch (str (nth source (inc character-pointer)))))]
         ;; check for 2 length char 
         (if (and can-look-ahead? (some #(= % ch2) allowed-tokens-2-chars))
           (recur (+ character-pointer 2) [(conj tokens (lex->token ch2 character-pointer)) errors])
           (do
             ;; check for string literal
             (if (and can-look-ahead? (= ch "\""))
               (let [string (get-until #(not= % "\"") source (inc character-pointer))]
                 (recur (+ 2 character-pointer (count string)) [(conj tokens (lex->token string character-pointer)) errors]))
               (do
                 ;; check for 1 length char 
                 (if (some #(= % ch) allowed-tokens)
                   (recur (+ character-pointer (count ch)) [(conj tokens (lex->token ch character-pointer)) errors])
                   (do
                     ;; check for literals : numbers     
                     (if (digit? ch)
                       (let [literal (get-until digit? source character-pointer)]
                         (recur (+ character-pointer (count literal)) [(conj tokens (lex->token literal character-pointer)) errors]))
                       ;; identifers
                       (if (identifier-characters? ch)
                         (let [identifier (get-until #(or (digit? %1) (identifier-characters? %1)) source character-pointer)]

                           (recur (+ character-pointer (count identifier)) [(conj tokens (lex->token identifier character-pointer)) errors]))
                         ;; no matchinng clause
                         (recur (inc character-pointer) [tokens errors]))))))))))))
   0 [[] []]))
(scan "hell=903 open==0hel=o3 \"ok\" 30=P87")
(identifier-characters? "3")
