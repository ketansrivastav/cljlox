(ns celjlox.lexer)

(defn scan
  [source]
  ((fn scan*
     [character-pointer [tokens errors]]
     "recursivly lex one character -> token at a time"
     (if (>= character-pointer (count source))
       [tokens errors]
       (let [ch (nth source character-pointer)]
         (print ":" ch)
         (recur (inc character-pointer) [tokens errors]))))
   0 [[:token] [:error]]))

(scan "hello
helo
")
