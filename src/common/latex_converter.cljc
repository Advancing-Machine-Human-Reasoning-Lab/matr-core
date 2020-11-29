(ns common.latex-converter
  (:require [clojure.string :as s]
            #?(:cljs [cljs.reader :as edn :refer [read-string]]
               :clj [clojure.edn :as edn :refer [read-string]])))

(declare s-exp->fn-m s-exp->fn s-exp->latex get-fn)

;; Test strings/axioms
(def axioms ["(EXISTS2 psi (PROVES PA (IFF psi (NOT (Proves (QB psi))))))"
             "(FORALL2 psi (AND (PROVES PA (Opposite (QB psi) (QB (NOT psi)))) (Opposite (QB psi) (QB (NOT psi)))))"
             "(FORALL2 phi (IMPLIES (PROVES PA phi) (Proves (QB phi))))"
             "(FORALL2 phi (IMPLIES (PROVES PA (Proves (QB phi))) (Proves (QB phi))))"
             "(FORALL n (FORALL m (IMPLIES (AND (Opposite n m) (Proves n)) (NOT (Proves m)))))"
             "(FORALL2 phi (FORALL2 psi (PROVES PA (IMPLIES (Proves (QB (IMPLIES phi psi))) (IMPLIES (Proves (QB phi)) (Proves (QB psi)))))))"
             "(FORALL2 phi (PROVES PA (IMPLIES (Proves (QB phi)) (Proves (QB (Proves (QB phi)))))))"
             "(NOT (EXISTS2 phi (AND (PROVES PA phi) (PROVES PA (NOT phi)))))"
             "(EXISTS2 phi (AND (NOT (PROVES PA phi)) (NOT (PROVES PA (NOT phi)))))"
             "(IMPLIES (PROVES PA (FORALL n (FORALL m (IMPLIES (AND (Opposite n m) (Proves n)) (NOT (Proves m)))))) (NOT (NOT (EXISTS2 p (AND (PROVES PA p) (PROVES PA (NOT p)))))))"])

(defn detect-paren-loss
  "Returns single closing paren if read-string drops closing paren due to not finding a matching open paren. Necessary for handling when an s-expression is split into two strings such that the outer pair of parens is no longer contained within a single string."
  [^String sym]
  (if (and (= (.-length (str (read-string sym)))
              (dec (.-length sym)))
           (= (last sym) \)))
    ")"
    ""))

(defn paren-loss?
  "Returns single closing paren if read-string drops closing paren due to not finding a matching open paren. Necessary for handling when an s-expression is split into two strings such that the outer pair of parens is no longer contained within a single string."
  [^String sym]
  (true? (and (= (.-length (str (read-string sym)))
                 (dec (.-length sym)))
              (= (last sym) \)))))

(defn ends-in-urcorner? [^String sym]
  (not (nil? (re-find #".*\\urcorner" sym))))

(defn close-parens-needed
  "Returns number of open parens - number of close parens or 0 if there are more close parens than open parens."
  [^String sym]
  (let [open-paren-count (count (re-seq #"[\(]" sym))
        close-paren-count (count (re-seq #"[\)]" sym))]
    (if (> open-paren-count close-paren-count)
      (- open-paren-count close-paren-count)
      0)))

(defn strip-first+last
  "Takes in string and returns substring with first and last indices removed."
  [^String string]
  (subs string 1 (dec (.-length string))))

(defn strip-outer-braces
  "Takes in string and returns substring with outer-most enclosing curly braces removed. Curly braces need not surround entire expression or be the beginning or end of string. If multiple non-nested curly braces exist, function will fail to remove proper pair, though function will work properly if all curly brace pairs are nested within each other."
  [^String sym]
  (let [first-index (s/index-of sym \{)
        last-index (s/last-index-of sym \})]
    (if (and first-index last-index)
      (str
       (subs sym (inc first-index) last-index) (subs sym (inc last-index)))
      sym)))

(defn re-pat-greek-sym
  "Generate re-pattern based on a greek symbol sym appearing in a string as either \"s\", \"(s)\", or \"{s}\"."
  [^String sym] (re-pattern (str "[\\s|\\(|\\{]?" sym "[\\s|\\)|\\}]?")))

(def greek-map {"alpha"    "\\alpha"
                "beta"     "\\beta"
                "gamma"    "\\gamma"
                "Gamma"    "\\Gamma"
                "delta"    "\\delta"
                "Delta"    "\\Delta"
                "epsilon"  "\\epsilon"
                "zeta"     "\\zeta"
                "eta"      "\\eta"
                "theta"    "\\theta"
                "Theta"    "\\Theta"
                "iota"     "\\iota"
                "kappa"    "\\kappa"
                "lambda"   "\\lambda"
                "Lambda"   "\\Lambda"
                "mu"       "\\mu"
                "nu"       "\\nu"
                "xi"       "\\xi"
                "Xi"       "\\Xi"
                "pi"       "\\pi"
                "Pi"       "\\Pi"
                "rho"      "\\rho"
                "sigma"    "\\sigma"
                "Sigma"    "\\Sigma"
                "tau"      "\\tau"
                "upsilon"  "\\upsilon"
                "Upsilon"  "\\Upsilon"
                "phi"      "\\phi"
                "Phi"      "\\Phi"
                "chi"      "\\chi"
                "psi"      "\\psi"
                "Psi"      "\\Psi"
                "omega"    "\\omega"
                "Omega"    "\\Omega"})

(defn greek?
  "Returns true if var ∈ (keys greek-map), false otherwise"
  [var]
  (not (nil? (some #(= % var) (keys greek-map)))))
; 09/19
(defn var->latex-text [sym]
  (let [[butlast-s last-s] (map (partial apply str) (split-at (dec (count sym)) sym))]
    (if (= last-s ")")
      (str "\\text{" butlast-s "}" last-s)
      (str "\\text{" butlast-s last-s "}"))))

(defn prefix->infix
  "Takes in a (string) arithmetic s-expression and outputs its infix equivalent (as a list)"
  [prefix-s]
  (let [s-exp (read-string prefix-s)
        func (first s-exp)]
    (loop [cur-s-exp (rest s-exp)
           infix '()]
      (if (= (count cur-s-exp) 1)
        (reverse (conj infix (first cur-s-exp)))
        (recur (rest cur-s-exp)
               (conj infix (first cur-s-exp) func))))))

(defn infix->latex
  "Takes in a (non-string) arithmetic infix expression and outputs its latex equivalent string"
  [infix]
  (str "$$" infix "$$"))

(defn greek-syms->latex
  "Takes in a (string) s-expression and replaces all greek variables with their latex equivalent commands."
  [^String s-exp]
  (loop [greek-syms greek-map
         replaced-s-exp s-exp]
    (if (empty? greek-syms)
      replaced-s-exp
      (recur (rest greek-syms)
             ;; If current greek symbol is found in replaced-s-exp, replace with latex
             ;; cmd.
             (if (some #(= % (key (first greek-syms))) (s/split s-exp #" |\(|\)|\{|\}"))
               (s/replace replaced-s-exp
                          (key (first greek-syms))
                          (val (first greek-syms)))
               replaced-s-exp)))))

;; Not currently in use
(defn drop-until-parens
  "Takes in string and drops beginning of string with opening parens as the delimiter."
  [exp]
  (apply str (drop-while #(not= % \() exp)))

(defn latex-text?
  "Returns true if string begins with \"(\\t\""
  [sym] (s/starts-with? sym "(\\t"))

(defn first-exp->getfn
  "Takes in s-expression as string and returns appropriate function to be applied to first function in s-expression."
  [sym]
  (if (latex-text? sym) #'identity
      (-> sym read-string str get-fn)))

(defn first-exp->latex
  "Takes in s-expression as string and returns a list of two strings with the first string containing the equivalent latex representation for the first function in the s-expression and the second string containing the remainder of the s-expression unchanged. If a non-function is passed (such as a single variable) as input, the input will be returned as output."
  [sym]
  ((first-exp->getfn sym) sym))

(defn get-fn
  "Takes in a (string) s-expression and returns the appropriate function to be applied to parse the function."
  [^String s-exp-s]
  (if (not (s/blank? s-exp-s))
    (if (re-find #".*\\urcorner" s-exp-s)
      #'identity
      ;; bracelss-s has outer curly braces stripped if present in s-exp-s
      (let [braceless-s (if (= (first s-exp-s) \{)
                          (strip-first+last s-exp-s)
                          s-exp-s)]
        (if (and (list? (read-string braceless-s)) (> (count (read-string braceless-s)) 1))
          (-> braceless-s read-string first s-exp->fn)
          (if (or (greek? braceless-s) (s/starts-with? braceless-s "Z_"))
            #'identity
            #'var->latex-text))))))


;; Unary functions


(defn unary-fn->latex
  "Generic unary function that takes in a (string) s-expression and a (string) latex-cmd and returns the latex equivalent expression."
  [s-exp-s latex-cmd]
  (let [s-exp (read-string s-exp-s) ; convert string->list
        arg (last s-exp)]
    (list (str latex-cmd " ") (str arg))))

(defn multi-arity-fn->latex [s-exp-s latex-cmd]
  (let [[f & rem-args] (read-string s-exp-s) ; convert string->list
        full-rem-args (str (s/join ", " (map (comp s-exp->latex str) rem-args)))]
    (str latex-cmd " (" full-rem-args ")")))

(defn predicate-fn->latex [s-exp-s]
  (let [[f & rem-args] (read-string s-exp-s)
        latex-cmd (if (greek? (str f)) (greek-map (str f)) (str "\\text{" f "}"))] ; convert string->list
    (multi-arity-fn->latex s-exp-s latex-cmd)))

(defn not->latex
  "Takes in (string) s-expression that begins with \"NOT\" and converts into string with exists statement in latex format and arguments unchanged."
  [not-s]
  (unary-fn->latex not-s "\\neg"))

(defn Proves->latex
  "Takes in (string) s-expression that begins with \"Proves\" and converts into string with exists statement in latex format and arguments unchanged."
  [prvs]
  (let [[prv arg] (unary-fn->latex prvs "\\text{Prv}_{\\text{PA}}")]
    (list prv (str "(" (s-exp->latex arg true) ")"))))

;; Need to Change "\\QB{arg}" -> "\\ulcorner arg \\urcorner"
(defn QB->latex
  "Takes in (string) s-expression that begins with \"QB\" and converts into string with exists statement in latex format and arguments unchanged."
  [QB-s]
  (let [latex-cmd "\\ulcorner"
        [QB-latex arg] (unary-fn->latex QB-s latex-cmd)] ; no curly braces around arg
    (list QB-latex (str "{" (s-exp->latex arg true) "} \\urcorner"))))



;; Modal functions


(defn first-arg-modal? [s-exp]
  (and (seqable? s-exp)
       (contains? #{'OR 'AND 'IFF 'IMPLIES '+ '> '<} (first s-exp))))

(defn modal-fn->latex [s-exp-s latex-cmd]
  "Generic modal function that takes in a (string) s-expression and a (string) latex-cmd and returns a list of two strings with the first string containing the latex equivalent of the function and its first argument and the second string containing the second argument unchanged."
  (let [[s-exp-arg1 & rem-args] (prefix->infix s-exp-s)
        latex-arg1 (s-exp->latex (str s-exp-arg1) true)
        last-arg (str (last rem-args))]
    (list
     (if (first-arg-modal? s-exp-arg1)
       (str "(" latex-arg1 ") " latex-cmd " ")
       (str latex-arg1 " " latex-cmd " "))
     last-arg)))

(defn iff->latex
  "Takes in (string) s-expression that begins with \"IFF\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [iff-s]
  (modal-fn->latex iff-s "\\leftrightarrow"))

(defn and->latex
  "Takes in (string) s-expression that begins with \"AND\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [and-s]
  (modal-fn->latex and-s "\\wedge"))

(defn or->latex
  "Takes in (string) s-expression that begins with \"OR\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [or-s]
  (modal-fn->latex or-s "\\vee"))

(defn implies->latex
  "Takes in (string) s-expression that begins with \"IMPLIES\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [imp-s]
  (modal-fn->latex imp-s "\\rightarrow"))

(defn +->latex
  "Takes in (string) s-expression that begins with \"+\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [plus-s]
  (modal-fn->latex plus-s "+"))

(defn gt->latex
  "Takes in (string) s-expression that begins with \">\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [gt-s]
  (modal-fn->latex gt-s ">"))

(defn lt->latex
  "Takes in (string) s-expression that begins with \">\" and converts into a list of two strings with the first string consisting of the implies statement and first argument in latex format and and the second string consisting of the second argument unchanged."
  [lt-s]
  (modal-fn->latex lt-s "<"))

;; Binary functions


(defn binary-fn-curly->latex
  "Generic binary function that takes in a (string) s-expression and a (string) latex-cmd and returns a list of two strings. First string consists of function and first argument to s-expression in latex-format surrounded in curly braces and the second string consists of the second argument unchanged.
  Example:
    Input: \"(EXISTS2 psi (PROVES PA (IFF psi (NOT (Proves (QB psi))))))\" \"\\exists_\"
    Output: (\"\\exists_{psi} \" \"(PROVES PA (IFF psi (NOT (Proves (QB psi)))))\")"
  [^String s-exp-s ^String latex-cmd]
  (let [s-exp (read-string s-exp-s) ; convert string->list
        first-arg (str (second s-exp)) ; underscore for exists
        full-rem-args (str (rest (rest s-exp))) ; extra pair of parens
        trimmed-rem-args (subs full-rem-args 1 (dec (.-length full-rem-args)))]
    (list (str latex-cmd "{" (s-exp->latex first-arg true) "} ") trimmed-rem-args)))

(defn exists2->latex
  "Takes in (string) s-expression that begins with \"EXISTS2\" and returns a list of two strings with the first string consisting of the exists statement and first argument in latex format and the second string consisting of the second argument unchanged."
  [ex2]
  (binary-fn-curly->latex ex2 "\\exists_"))

(defn forall2->latex
  "Takes in (string) s-expression that begins with \"FORALL2\" and returns a list of two strings with the first string consisting of the forall2 statement and first argument in latex format and the second string consisting of the second argument unchanged."
  [forall]
  (binary-fn-curly->latex forall "\\forall_"))

(defn binary-fn-paren->latex
  "Generic binary function that takes in a (string) s-expression and a (string) latex-cmd and returns a list of two strings. The first string consists of the latex-formatted function with opening paren, the first argument to the function, and a comma with a space. The second string consists of the second argument unchanged and a closing paren.
  Example:
    Input: \"(PROVES PA (IFF psi (NOT (Proves (QB psi)))))\" \"\\square\"
    Output: (\"\\square (PA, \" \"(IFF psi (NOT (Proves (QB psi)))))\")"
  [^String s-exp-s ^String latex-cmd]
  (let [s-exp (read-string s-exp-s) ; convert string->list
        first-arg (str (second s-exp))
        full-rem-args (str (rest (rest s-exp))) ; extra pair of parens
        trimmed-rem-args (subs full-rem-args 1 (dec (.-length full-rem-args)))]
    (list (str latex-cmd " (" (s-exp->latex first-arg true) ", ") (str trimmed-rem-args ")"))))

(defn PROVES->latex
  "Takes in (string) s-expression that begins with \"PROVES\" and returns a list of two strings with the first string consisting of the PROVES statement and first argument in latex format and the second string consisting of the second argument unchanged."
  [prvs]
  (binary-fn-paren->latex prvs "\\square"))

(defn opposite->latex
  "Takes in (string) s-expression that begins with \"Opposite\" and returns a list of two strings with the first string consisting of the Opposite statement and first argument in latex format and the second string consisting of the second argument unchanged."
  [opp]
  (binary-fn-paren->latex opp "\\text{Opposite}"))

(defn forall->latex
  "Takes in (string) s-expression that begins with \"FORALL\" and converts into string with FORALL statement and first argument in latex format and second argument unchanged."
  [forall]
  (binary-fn-paren->latex forall "\\forall"))

(defn strip-parens
  "Takes in string and drops opening and closing parentheses."
  [exp]
  (re-find #"[^\(?=\)]+" exp))

(defn arithmetic?
  "Takes in a (non-string) s-expression and returns true if the expression is arithmetic, an equality, or an inequality and false otherwise."
  [s-exp]
  (case (str (first s-exp))
    ("+" "-" "/" "*" "<" ">" "=") true
    false))

;; S-expression->function-map: maps axiom symbols to their respective functions for conversion to
;; latex commands
(def s-exp->fn-m {'IFF      #'iff->latex
                  'NOT      #'not->latex
                  'QB       #'QB->latex
                  'EXISTS2  #'exists2->latex
                  'PROVES   #'PROVES->latex
                  'Proves   #'Proves->latex
                  'AND      #'and->latex
                  'OR       #'or->latex
                  'FORALL   #'forall2->latex
                  'FORALL2  #'forall2->latex
                  'Opposite #'opposite->latex
                  'IMPLIES  #'implies->latex
                  '+        #'+->latex
                  'plus     #'+->latex
                  '>        #'gt->latex
                  '<        #'lt->latex})

(defn s-exp->fn [k]
  (if (nil? (s-exp->fn-m k)) #'predicate-fn->latex (s-exp->fn-m k)))

(def unary-fns [QB->latex])

(def unary-str ["\\ulcorner" "\\neg" "\\square" "\\forall_"])

(defn unary? [f] (some #(= % f) unary-fns))

(defn unary-str? [^String sym]
  (let [trim-sym
        (if (= (first (s/trim sym)) \()
          (subs (s/trim sym) 1) (s/trim sym))]
    (true? (some #(s/starts-with? trim-sym %) unary-str))))

(defn debug-s-exp->latex
  "Takes in a (string) s-expression and outputs an equivalent expression formatted for latex."
  ([axiom]
   (s-exp->latex axiom false))
  ;; If inner? is true then wrapped "$$" symbols are omitted
  ([^String axiom inner?]
   (loop [parsed ""
          remaining axiom
          [first-exp second-exp]
          (if (list? ((-> axiom read-string str get-fn) axiom))
            ((-> axiom read-string str get-fn) axiom)
            (list ((-> axiom read-string str get-fn) axiom) nil))]
     (do (prn first-exp second-exp)
         (if (nil? second-exp)
           (let [unclosed-exp (if (= (get parsed 0) \()
                                (str (subs parsed 1) first-exp)
                                (str parsed first-exp))]
             (if inner?
               (str unclosed-exp (apply str (repeat (close-parens-needed unclosed-exp) ")")))
               (greek-syms->latex (str "$$" unclosed-exp (apply str (repeat (close-parens-needed unclosed-exp) ")")) "$$"))))
           (recur
            (if (unary-str? (first (s/split (str first-exp) #" ")))
              (str parsed first-exp)
              (str parsed "(" first-exp))
            (str second-exp)
            (if (ends-in-urcorner? second-exp)
              (list second-exp nil)
              (let [second-exp-no-curls
                    (if (= (get second-exp 0) \{)
                      (strip-outer-braces second-exp)
                      second-exp)]
                ;; Need to concatenate \\urcorner onto end of QB->latex
                (if (list? ((-> second-exp-no-curls read-string str get-fn) second-exp-no-curls))
                  ((-> second-exp-no-curls read-string str get-fn) second-exp-no-curls)
                  (list ((-> second-exp-no-curls read-string str get-fn) second-exp-no-curls) nil))))))))))

(defn s-exp->latex
  "Takes in a (string) s-expression and outputs an equivalent expression formatted for latex."
  ([axiom]
   (s-exp->latex axiom false))
  ;; If inner? is true then wrapped "$$" symbols are omitted
  ([^String axiom inner?]
   (loop [parsed ""
          remaining axiom
          [first-exp second-exp]
          (if (list? (first-exp->latex axiom))
            (first-exp->latex axiom)
            (list (first-exp->latex axiom) nil))]
     (if (nil? second-exp)
       (let [unclosed-exp (if (= (first parsed) \()
                            (str (subs parsed 1) first-exp)
                            (str parsed first-exp))]
         (if inner?
           (str unclosed-exp (apply str (repeat (close-parens-needed unclosed-exp) ")")))
           (greek-syms->latex (str unclosed-exp (apply str (repeat (close-parens-needed unclosed-exp) ")"))))))
       (recur
        (if (unary-str? (first (s/split (str first-exp) #" ")))
          (str parsed first-exp)
          (str parsed "(" first-exp))
        (str second-exp)
        (if (ends-in-urcorner? second-exp)
          (list second-exp nil)
          (let [second-exp-no-curls
                (if (= (get second-exp 0) \{)
                  (strip-outer-braces second-exp)
                  second-exp)]
            ;; Need to concatenate \\urcorner onto end of QB->latex
            (if (list? (first-exp->latex second-exp-no-curls))
              (first-exp->latex second-exp-no-curls)
              (list (first-exp->latex second-exp-no-curls) nil)))))))))

(defn get-avg [coll] (let [size (double (count coll))]
                       (/ (reduce + coll) size)))

;; Benchmarking
(defn compare-converter-times [f1 f2 runs]
  (let [t-f-m
        (frequencies
         (map >
              (for [axiom axioms] ;; Get avg times of executing f1 on all
                (get-avg          ;; axioms runs times
                 (for [i (range runs)]
                   (read-string
                    (first
                     (re-find #"[+-]?([0-9]*[.])?[0-9]+"
                              (re-find #"Elapsed time: .*"
                                       (with-out-str
                                         (time (f1 axiom))))))))))
              (for [axiom axioms] ;; Get avg times of executing f2 on all
                (get-avg          ;; axioms runs times
                 (for [i (range runs)]
                   (read-string
                    (first
                     (re-find #"[+-]?([0-9]*[.])?[0-9]+"
                              (re-find #"Elapsed time: .*"
                                       (with-out-str
                                         (time
                                          (f2 axiom))))))))))))]
    (prn
     (apply str "f2 is faster than f1 for " (t-f-m true) " out of "
            (reduce + (vals t-f-m)) " axioms"))))


;; Test functions


(defn get-time-of-str-conversion [n]
  (time (str (repeat n \a)))
  nil)

(defn get-time-of-pr-str-conversion [n]
  (time (pr-str (repeat n \a)))
  nil)
