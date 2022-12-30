(ns celjlox.lexer)
(def vocabulary [{:token "<"
                  :type ::LESS-THAN}
                 {:token ">"
                  :type ::GREATER-THAN}
                 {:token "="
                  :type ::EQUAL}
                 {:token "+"
                  :type ::ADD}
                 {:token "-"
                  :type ::MINUS}
                 {:token "=="
                  :type ::EQUALS-TO}
                 {:keyword "and"
                  :type ::AND}
                 {:keyword "then"
                  :type ::THEN}
                 {:keyword "class"
                  :type ::class}])
(def allowed-keywords
  #{"and"
    "class"
    "else"
    "false"
    "for"
    "fun"
    "if"
    "nil"
    "or"
    "print"
    "return"
    "super"
    "this"
    "true"
    "var"
    "while"})
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
  ([token-type position]
   (lex->token token-type nil position))
  (
   [token-type literal position]
   {:token-type token-type
    :literal literal
    :postiion position}))

(defn scan
  [source]
  ((fn scan*
     [character-pointer [tokens errors]]
     "recursivly lex one character -> token at a time"
     (if (>= character-pointer (count source))
       [tokens errors]
       (let [ch (str (nth source character-pointer))
             can-look-ahead? (< (inc character-pointer) (count source))
             ch2 (if can-look-ahead? (str ch (str (nth source (inc character-pointer)))))
             allowed-tokens-1-ch
               (filter #(and (contains? % :token) (= (count (:token %)) 1)) vocabulary)
             allowed-tokens-2-ch
               (filter #(and (contains? % :token) (= (count (:token %)) 2)) vocabulary)]
         ;; check for 2 length char 
         (if (and can-look-ahead? (some #(= % ch2) (map :token allowed-tokens-2-ch)))
           (let [token (->> allowed-tokens-2-ch
                            (filter #(= (:token %) ch2))
                            (first)
                            (:type))]
             (recur (+ character-pointer (count ch2)) [(conj tokens (lex->token token character-pointer)) errors]))

           (do
             ;; check for string literal
             (if (and can-look-ahead? (= ch "\""))
               (let [string (get-until #(not= % "\"") source (inc character-pointer))]
                 (recur (+ 2 character-pointer (count string)) [(conj tokens (lex->token ::STRING string character-pointer)) errors]))
               (do
                 ;; check for 1 length char 
                 (if (some #(= % ch) (map :token allowed-tokens-1-ch))
                   (let [token (->> allowed-tokens-1-ch
                                    (filter #(= (:token %) ch))
                                    (first)
                                    (:type))]
                     (recur (+ character-pointer (count ch)) [(conj tokens (lex->token token character-pointer)) errors]))
                   (do
                     ;; check for literals : numbers     
                     (if (digit? ch)
                       (let [literal (get-until digit? source character-pointer)]
                         (recur (+ character-pointer (count literal)) [(conj tokens (lex->token ::NUMBER literal character-pointer)) errors]))
                       ;; identifers and keywords;
                       (if (identifier-characters? ch)
                         (let [identifier (get-until #(or (digit? %1) (identifier-characters? %1)) source character-pointer)
                               is-keyword? (some #(= (:keyword %1) identifier) (filter :keyword vocabulary))
                               keywords (filter :keyword vocabulary)]
                           (recur (+ character-pointer (count identifier)) [(conj tokens
                                                                                  (if is-keyword?
                                                                                    (lex->token (->> keywords
                                                                                                     (filter #(= (:keyword %) identifier))
                                                                                                     (first)
                                                                                                     (:type)) character-pointer)
                                                                                    (lex->token ::IDENTIFIER identifier character-pointer))) errors]))
                         ;; no matchinng clause
                         (recur (inc character-pointer) [tokens errors]))))))))))))
   0 [[] []]))
(scan "hell=903 open==0hel=o3 \"ok\" 30=P87 if ifr and then thenedn")
