;;; ~/.configs/doom.d/+symbols.el -*- lexical-binding: t; -*-

;;;  -*- lexical-binding: t; -*-

(require 'dash)

;; Make sure you have the fonts in fonts.tar.gz installed
(set-fontset-font t '(#xe100 . #xe16f) "Iosevka")
(defun setup-iosevka-ligatures ()
  (setq prettify-symbols-alist
        (append prettify-symbols-alist '(
                                         ;; Double-ended hyphen arrows ----------------
                                         ("<->" . #Xe100)
                                         ("<-->" . #Xe101)
                                         ("<--->" . #Xe102)
                                         ("<---->" . #Xe103)
                                         ("<----->" . #Xe104)

                                         ;; Double-ended equals arrows ----------------
                                         ("<=>" . #Xe105)
                                         ("<==>" . #Xe106)
                                         ("<===>" . #Xe107)
                                         ("<====>" . #Xe108)
                                         ("<=====>" . #Xe109)

                                         ;; Double-ended asterisk operators ----------------
                                         ("<**>" . #Xe10a)
                                         ("<***>" . #Xe10b)
                                         ("<****>" . #Xe10c)
                                         ("<*****>" . #Xe10d)

                                         ;; HTML comments ----------------
                                         ("<!--" . #Xe10e)
                                         ("<!---" . #Xe10f)

                                         ;; Three-char ops with discards ----------------
                                         ("<$" . #Xe110)
                                         ("<$>" . #Xe111)
                                         ("$>" . #Xe112)
                                         ("<." . #Xe113)
                                         ("<.>" . #Xe114)
                                         (".>" . #Xe115)
                                         ("<*" . #Xe116)
                                         ("<*>" . #Xe117)
                                         ("*>" . #Xe118)
                                         ("<\\" . #Xe119)
                                         ("<\\>" . #Xe11a)
                                         ("\\>" . #Xe11b)
                                         ("</" . #Xe11c)
                                         ("</>" . #Xe11d)
                                         ("/>" . #Xe11e)
                                         ("<\"" . #Xe11f)
                                         ("<\">" . #Xe120)
                                         ("\">" . #Xe121)
                                         ("<'" . #Xe122)
                                         ("<'>" . #Xe123)
                                         ("'>" . #Xe124)
                                         ("<^" . #Xe125)
                                         ("<^>" . #Xe126)
                                         ("^>" . #Xe127)
                                         ("<&" . #Xe128)
                                         ("<&>" . #Xe129)
                                         ("&>" . #Xe12a)
                                         ("<%" . #Xe12b)
                                         ("<%>" . #Xe12c)
                                         ("%>" . #Xe12d)
                                         ("<@" . #Xe12e)
                                         ("<@>" . #Xe12f)
                                         ("@>" . #Xe130)
                                         ("<#" . #Xe131)
                                         ("<#>" . #Xe132)
                                         ("#>" . #Xe133)
                                         ("<+" . #Xe134)
                                         ("<+>" . #Xe135)
                                         ("+>" . #Xe136)
                                         ("<-" . #Xe137)
                                         ("<->" . #Xe138)
                                         ("->" . #Xe139)
                                         ("<!" . #Xe13a)
                                         ("<!>" . #Xe13b)
                                         ("!>" . #Xe13c)
                                         ("<?" . #Xe13d)
                                         ("<?>" . #Xe13e)
                                         ("?>" . #Xe13f)
                                         ("<|" . #Xe140)
                                         ("<|>" . #Xe141)
                                         ("|>" . #Xe142)
                                         ("<:" . #Xe143)
                                         ("<:>" . #Xe144)
                                         (":>" . #Xe145)

                                         ;; Colons ----------------
                                         ("::" . #Xe146)
                                         (":::" . #Xe147)
                                         ("::::" . #Xe148)

                                         ;; Arrow-like operators ----------------
                                         ("->" . #Xe149)
                                         ("->-" . #Xe14a)
                                         ("->--" . #Xe14b)
                                         ("->>" . #Xe14c)
                                         ("->>-" . #Xe14d)
                                         ("->>--" . #Xe14e)
                                         ("->>>" . #Xe14f)
                                         ("->>>-" . #Xe150)
                                         ("->>>--" . #Xe151)
                                         ("-->" . #Xe152)
                                         ("-->-" . #Xe153)
                                         ("-->--" . #Xe154)
                                         ("-->>" . #Xe155)
                                         ("-->>-" . #Xe156)
                                         ("-->>--" . #Xe157)
                                         ("-->>>" . #Xe158)
                                         ("-->>>-" . #Xe159)
                                         ("-->>>--" . #Xe15a)
                                         (">-" . #Xe15b)
                                         (">--" . #Xe15c)
                                         (">>-" . #Xe15d)
                                         (">>--" . #Xe15e)
                                         (">>>-" . #Xe15f)
                                         (">>>--" . #Xe160)
                                         ("=>" . #Xe161)
                                         ("=>=" . #Xe162)
                                         ("=>==" . #Xe163)
                                         ("=>>" . #Xe164)
                                         ("=>>=" . #Xe165)
                                         ("=>>==" . #Xe166)
                                         ("=>>>" . #Xe167)
                                         ("=>>>=" . #Xe168)
                                         ("=>>>==" . #Xe169)
                                         ("==>" . #Xe16a)
                                         ("==>=" . #Xe16b)
                                         ("==>==" . #Xe16c)
                                         ("==>>" . #Xe16d)
                                         ("==>>=" . #Xe16e)
                                         ("==>>==" . #Xe16f)
                                         ("==>>>" . #Xe170)
                                         ("==>>>=" . #Xe171)
                                         ("==>>>==" . #Xe172)
                                         (">=" . #Xe173)
                                         (">==" . #Xe174)
                                         (">>=" . #Xe175)
                                         (">>==" . #Xe176)
                                         (">>>=" . #Xe177)
                                         (">>>==" . #Xe178)
                                         ("<-" . #Xe179)
                                         ("-<-" . #Xe17a)
                                         ("--<-" . #Xe17b)
                                         ("<<-" . #Xe17c)
                                         ("-<<-" . #Xe17d)
                                         ("--<<-" . #Xe17e)
                                         ("<<<-" . #Xe17f)
                                         ("-<<<-" . #Xe180)
                                         ("--<<<-" . #Xe181)
                                         ("<--" . #Xe182)
                                         ("-<--" . #Xe183)
                                         ("--<--" . #Xe184)
                                         ("<<--" . #Xe185)
                                         ("-<<--" . #Xe186)
                                         ("--<<--" . #Xe187)
                                         ("<<<--" . #Xe188)
                                         ("-<<<--" . #Xe189)
                                         ("--<<<--" . #Xe18a)
                                         ("-<" . #Xe18b)
                                         ("--<" . #Xe18c)
                                         ("-<<" . #Xe18d)
                                         ("--<<" . #Xe18e)
                                         ("-<<<" . #Xe18f)
                                         ("--<<<" . #Xe190)
                                         ("<=" . #Xe191)
                                         ("=<=" . #Xe192)
                                         ("==<=" . #Xe193)
                                         ("<<=" . #Xe194)
                                         ("=<<=" . #Xe195)
                                         ("==<<=" . #Xe196)
                                         ("<<<=" . #Xe197)
                                         ("=<<<=" . #Xe198)
                                         ("==<<<=" . #Xe199)
                                         ("<==" . #Xe19a)
                                         ("=<==" . #Xe19b)
                                         ("==<==" . #Xe19c)
                                         ("<<==" . #Xe19d)
                                         ("=<<==" . #Xe19e)
                                         ("==<<==" . #Xe19f)
                                         ("<<<==" . #Xe1a0)
                                         ("=<<<==" . #Xe1a1)
                                         ("==<<<==" . #Xe1a2)
                                         ("=<" . #Xe1a3)
                                         ("==<" . #Xe1a4)
                                         ("=<<" . #Xe1a5)
                                         ("==<<" . #Xe1a6)
                                         ("=<<<" . #Xe1a7)
                                         ("==<<<" . #Xe1a8)

                                         ;; Monadic operators ----------------
                                         (">=>" . #Xe1a9)
                                         (">->" . #Xe1aa)
                                         (">-->" . #Xe1ab)
                                         (">==>" . #Xe1ac)
                                         ("<=<" . #Xe1ad)
                                         ("<-<" . #Xe1ae)
                                         ("<--<" . #Xe1af)
                                         ("<==<" . #Xe1b0)

                                         ;; Composition operators ----------------
                                         (">>" . #Xe1b1)
                                         (">>>" . #Xe1b2)
                                         ("<<" . #Xe1b3)
                                         ("<<<" . #Xe1b4)

                                         ;; Lens operators ----------------
                                         (":+" . #Xe1b5)
                                         (":-" . #Xe1b6)
                                         (":=" . #Xe1b7)
                                         ("+:" . #Xe1b8)
                                         ("-:" . #Xe1b9)
                                         ("=:" . #Xe1ba)
                                         ("=^" . #Xe1bb)
                                         ("=+" . #Xe1bc)
                                         ("=-" . #Xe1bd)
                                         ("=*" . #Xe1be)
                                         ("=/" . #Xe1bf)
                                         ("=%" . #Xe1c0)
                                         ("^=" . #Xe1c1)
                                         ("+=" . #Xe1c2)
                                         ("-=" . #Xe1c3)
                                         ("*=" . #Xe1c4)
                                         ("/=" . #Xe1c5)
                                         ("%=" . #Xe1c6)

                                         ;; Logical ----------------
                                         ("/\\" . #Xe1c7)
                                         ("\\/" . #Xe1c8)

                                         ;; Semigroup/monoid operators ----------------
                                         ("<>" . #Xe1c9)
                                         ("<+" . #Xe1ca)
                                         ("<+>" . #Xe1cb)
                                         ("+>" . #Xe1cc)))))

(load! "prettify-utils") ;; https://github.com/Ilazki/prettify-utils.el/blob/master/prettify-utils.el

;; Grabbed this from https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/display/local/pretty-code/pretty-code.el
(defvar pretty-code-choices
  (-flatten
   (prettify-utils-generate
    ;; Functional
    (:lambda      "λ")
    (:def         "ƒ")
    (:composition "∘")

    ;; Types
    (:null        "∅")
    (:true        "𝕋") (:false       "𝔽")
    (:int         "ℤ") (:float       "ℝ")
    (:str         "𝕊")
    (:bool        "𝔹")

    ;; Flow
    (:not         "￢")
    (:in          "∈") (:not-in      "∉")
    (:and         "∧") (:or          "∨")
    (:for         "∀")
    (:some        "∃")
    (:return     "⟼") (:yield      "⟻")

    ;; Other
    (:tuple       "⨂")
    (:pipe        "")))

  "Options plist for `pretty-code-get-pairs'.")

;;; Core

(defun pretty-code-get-pairs (kwds)
  "Build an alist for prettify-symbols-alist from components from KWDS."
  (-non-nil
   (--map (when-let (major-mode-symbol (plist-get kwds it))
            (list major-mode-symbol
                  (plist-get pretty-code-choices it)))
          pretty-code-choices)))

(defun pretty-code-set-pairs (hook-pairs-alist)
  "Add hooks setting `prettify-symbols-alist' for many modes"
  (-each hook-pairs-alist
    (-lambda ((hook pretty-pairs))
      (add-hook hook
                (lambda ()
                  (setq prettify-symbols-alist pretty-pairs)
                  (setup-iosevka-ligatures)
                  (prettify-symbols-mode -1)
                  (prettify-symbols-mode +1))))))

;; Init the stuff

(setq js2-pretty-pairs
      (pretty-code-get-pairs
       '(
         ;; Functional
         :def
         "function"
         :lambda "() =>"
         :composition "compose"

         ;; Types
         :null "null"
         :true "true" :false "false"

         ;; Flow
         :not "!"
         :and "&&" :or "||"
         :for "for"
         :return "return"

         ;; Other
         :yield "import"))

      c-pretty-pairs
      (pretty-code-get-pairs
       '( ;; Functional
         ;; :def
         ;; "void "

         ;; Types
         :null "nullptr"
         :true "true" :false "false"
         :int "int" :float "float"
         :str "std::string"
         :bool "bool"

         ;; Flow
         :not "!"
         :and "&&" :or "||"
         :for "for"
         :return "return"
         :yield "#require"))

      ts-pretty-pairs
      (pretty-code-get-pairs
       '(
         ;;Functional
         :def
         "function"
         :lambda "() =>"
         :composition "compose"

         ;; Types
         :null "null"
         :true "true" :false "false"
         :int "number"
         :str "string"
         :bool "boolean"

         ;; Flow
         :not "!"
         :and "&&" :or "||"
         :for "for"
         :return "return" :yield "import"))
      py-pretty-pairs
      (pretty-code-get-pairs
       '(
         :def
         "def"
         :lambda "lambda"

         :null "None"
         :true "True" :false "False"
         :int "int" :str "str"
         :float "float"
         :bool "bool"

         :not "not"
         :in "in" :not-in "not in"
         :and "and" :or "or"
         :for "for"
         :return "return" :yield "yield"

         :tuple "tuple"))
      lisp-pretty-pairs
      (pretty-code-get-pairs
       '(:lambda "lambda"))
      elm-pretty-pairs
      (pretty-code-get-pairs
       '(
         :null
         "null"
         :true "true" :false "false"
         :int "Int" :str "String"
         :float "Float"
         :bool "Bool"

         :not "not"
         :and "&&" :or "||"))
      blank-pretty-pairs ;; Blank so the above ones apply
      (pretty-code-get-pairs
       '()))


(pretty-code-set-pairs `((js2-mode-hook ,js2-pretty-pairs)
                         (web-mode-hook ,js2-pretty-pairs)
                         (typescript-mode-hook ,ts-pretty-pairs)
                         (c-mode-hook ,c-pretty-pairs)
                         (c++-mode-hook ,c-pretty-pairs)
                         (python-mode-hook ,py-pretty-pairs)
                         (emacs-lisp-mode-hook ,lisp-pretty-pairs)
                         (haskell-mode-hook ,blank-pretty-pairs)
                         (elixir-mode-hook ,blank-pretty-pairs)
                         (elm-mode-hook ,elm-pretty-pairs)
                         (reason-mode-hook ,elm-pretty-pairs))) ;; temp

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)
