(defun input-method/single-inactivate ()
  "Deactivates the current input method, and removes itself from the
`post-command-hook'; used to implement the single input method."
  (unless (memq this-command '(eli-input-method/single
                               isearch-toggle-eli-input-method/single))
    (remove-hook 'post-command-hook 'input-method/single-inactivate t)
    (inactivate-input-method)))

(defun set-input-method/single (method)
  "Sets `method' as the current input, which will be used only for one input."
  (if current-input-method
    (let ((this-command nil)) (input-method/single-inactivate))
    (progn (set-input-method method)
           ;; if the command switches to a different buffer the input
           ;; method will stay in effect, but will be disabled right
           ;; after switching back to it
           (add-hook 'post-command-hook 'input-method/single-inactivate t t))))

(defun eli-input-method/single ()
  "Set the `eli' input method for just one character."
  (interactive)
  (set-input-method/single "eli"))

(defun isearch-toggle-eli-input-method/single ()
  "Set the `eli' input method for just one character in isearch-mode."
  (interactive)
  (if current-input-method
    (let ((this-command nil)) (inactivate-input-method))
    (let ((default-input-method "eli"))
      (isearch-toggle-input-method)
      (add-hook 'post-command-hook 'input-method/single-inactivate t t))))

(add-hook 'isearch-mode-end-hook
          ;; for some reason isearch with the above leaves this set
          '(lambda () (setq input-method-function nil)))

(define-keys
  'global '([(meta return)] eli-input-method/single)
  isearch-mode-map '([(meta return)] isearch-toggle-eli-input-method/single))

(require 'quail)

(quail-define-package "eli" "UTF-8" "--<<<E>>>--" t "Eli's input method")

(quail-define-rules
;; convenient mnemonics
    ("\\"     #x03BB)
    ("``"     #x201C)
    ("''"     #x201D)
    ("`"      #x2018)
    ("'"      #x2019)
    ("<"      #x2329)
    (">"      #x232A)
    ("<<"     #x00AB)
    (">>"     #x00BB)
    ("[["     #X27E6)
    ("]]"     #X27E7)
    ("=="     #x2261)
    ("=def"   #x225D)
    ("=?"     #x225F)
    (":="     #x2254)
    ("/="     #x2260)
    ("/=="    #x2262)
    ("<="     #x2264)
    (">="     #x2265)
    ("/<"     #x226E)
    ("/>"     #x226F)
    ("/<="    #x2270)
    ("/>="    #x2271)
    ("|-"     #x22A2)
    ("|="     #x22A8)
    ("||-"    #x22A9)
    ("||"     #x2016)
    ("|->"    #x21A6)
    ("->"     #x2192)
    ("=>"     #x21D2)
    ("->>"    #x21A0)
    (">->"    #x21C9)
    ("<-"     #x2190)
    ("<="     #x21D0)
    ("<<-"    #x219E)
    ("<-<"    #x21C7)
    ("<->"    #x2194)
    ("<=>"    #x21D4)
    ("<~>"    #x21AD)
    ("---"    #x2014)
    ("--"     #x2013)
    ("..."    #x2026)
    ("+-"     #x00B1)
    ("-+"     #x2213)
    ("*"      #x2022)
    ("/\\"    #x2227)
    ("\\/"    #x2228)
    ("AND"    #x22C0)
    ("OR"     #x22C1)
    ("//\\\\" #x22C0)
    ("\\\\//" #x22C1)
    (":)"     [#x263A #x263B])
    (":("     #x2639)
    ("(C)"    #x00A9)
    ("(R)"    #x00AE)
    ("TM"     #x2122)
    ("all"    #x2200)
    ("sqrt"   #x221A)
    ;; additional subscripts and superscripts etc
    ("^0"     #x2070)
    ("^1"     #x00B9)
    ("^2"     #x00B2)
    ("^3"     #x00B3)
    ("^4"     #x2074)
    ("^5"     #x2075)
    ("^6"     #x2076)
    ("^7"     #x2077)
    ("^8"     #x2078)
    ("^9"     #x2079)
    ("^+"     #x207A)
    ("^-"     #x207B)
    ("^="     #x207C)
    ("^("     #x207D)
    ("^)"     #x207E)
    ("_0"     #x2080)
    ("_1"     #x2081)
    ("_2"     #x2082)
    ("_3"     #x2083)
    ("_4"     #x2084)
    ("_5"     #x2085)
    ("_6"     #x2086)
    ("_7"     #x2087)
    ("_8"     #x2088)
    ("_9"     #x2089)
    ("_+"     #x208A)
    ("_-"     #x208B)
    ("_="     #x208C)
    ("_("     #x208D)
    ("_)"     #x208E)
    ("sup0"   #x2070)
    ("sup4"   #x2074)
    ("sup5"   #x2075)
    ("sup6"   #x2076)
    ("sup7"   #x2077)
    ("sup8"   #x2078)
    ("sup9"   #x2079)
    ("sup+"   #x207A)
    ("sup-"   #x207B)
    ("sup="   #x207C)
    ("sup("   #x207D)
    ("sup)"   #x207E)
    ("sub0"   #x2080)
    ("sub1"   #x2081)
    ("sub2"   #x2082)
    ("sub3"   #x2083)
    ("sub4"   #x2084)
    ("sub5"   #x2085)
    ("sub6"   #x2086)
    ("sub7"   #x2087)
    ("sub8"   #x2088)
    ("sub9"   #x2089)
    ("sub+"   #x208A)
    ("sub-"   #x208B)
    ("sub="   #x208C)
    ("sub("   #x208D)
    ("sub)"   #x208E)
    ("1/2"    #x00BD)
    ("1/3"    #x2153)
    ("1/4"    #x00BC)
    ("1/5"    #x2155)
    ("1/6"    #x2159)
    ("1/8"    #x215B)
    ("2/3"    #x2154)
    ("2/5"    #x2156)
    ("3/4"    #x00BE)
    ("3/5"    #x2157)
    ("3/8"    #x215C)
    ("4/5"    #x2158)
    ("5/6"    #x215A)
    ("5/8"    #x215D)
    ("7/8"    #x215E)
    ;; "#" -> heavy
    ("#`"     #x275B)
    ("#'"     #x275C)
    ("#``"    #x275D)
    ("#''"    #x275E)
    ("#!"     #x2762)
    ("#<"     #x276E)
    ("#>"     #x276F)
    ("#->"    #x2794)
    ("#~>"    #x279C)
    ("#=>"    #x27A1)
    ("#>>"    #x27A0)
    ("#>>>" [#x27A2 #x27A3 #x27A4 #x27A5 #x27A6 #x27A7 #x27A8 #x27A9 #x27AA])
    ("#check" #x2714)
    ("#v"     #x2714)
    ("#x"     #x2716)
    ("#*"     #x2731)
    ("#+"     #x271A)
    ;; "," -> turned
    (",?"     #x00BF)
    (",!"     #x00A1)
    (",&"     #x214B)
    (",A"     #x2C6F) ; or #x2200 (forall)?
    (",a"     #x0250)
    (",E"     #x018E) ; or #x2203 (exists)?
    (",e"     #x01DD)
    (",F"     #x2132)
    (",g"     #x1D77)
    ;; (",G"     #x2141) ; sans-serif
    (",h"     #x0265) ; #xA78D?
    (",i"     #x1D09)
    (",k"     #x029E)
    (",L"     #xA780)
    (",l"     #xA781)
    (",M"     #x019C)
    (",m"     #x026F)
    (",r"     #x0279)
    ;; (",R"     #x1D1A) ; small one
    (",t"     #x0287)
    (",v"     #x028c)
    (",V"     #x0245)
    (",w"     #x028d)
    (",y"     #x028e)
    ;; (",Y"     #x2144) ; sans-serif
    (",not"   #x2319)

;;; ---------------------------------------------------------------------------
;;; from http://unicode.org/Public/MAPPINGS/VENDORS/MISC/SGML.TXT

("Aacgr"    #x0386) ; <ISOgrk2> # GREEK CAPITAL LETTER ALPHA WITH TONOS
("aacgr"    #x03AC) ; <ISOgrk2> # GREEK SMALL LETTER ALPHA WITH TONOS
("Aacute"   #x00C1) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH ACUTE
("aacute"   #x00E1) ; <ISOlat1> # LATIN SMALL LETTER A WITH ACUTE
("Abreve"   #x0102) ; <ISOlat2> # LATIN CAPITAL LETTER A WITH BREVE
("abreve"   #x0103) ; <ISOlat2> # LATIN SMALL LETTER A WITH BREVE
("Acirc"    #x00C2) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH CIRCUMFLEX
("acirc"    #x00E2) ; <ISOlat1> # LATIN SMALL LETTER A WITH CIRCUMFLEX
("acute"    #x00B4) ; <ISOdia> # ACUTE ACCENT
("Acy"      #x0410) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER A
("acy"      #x0430) ; <ISOcyr1> # CYRILLIC SMALL LETTER A
("AElig"    #x00C6) ; <ISOlat1> # LATIN CAPITAL LETTER AE
("aelig"    #x00E6) ; <ISOlat1> # LATIN SMALL LETTER AE
("Agr"      #x0391) ; <ISOgrk1> # GREEK CAPITAL LETTER ALPHA
("agr"      #x03B1) ; <ISOgrk1> # GREEK SMALL LETTER ALPHA
("Agrave"   #x00C0) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH GRAVE
("agrave"   #x00E0) ; <ISOlat1> # LATIN SMALL LETTER A WITH GRAVE
("alefsym"  #x2135) ; <HTMLsymbol> # ALEF SYMBOL
("aleph"    #x2135) ; <ISOtech> # ALEF SYMBOL
("Alpha"    #x0391) ; <HTMLsymbol> # GREEK CAPITAL LETTER ALPHA
("alpha"    #x03B1) ; <ISOgrk3> # GREEK SMALL LETTER ALPHA
("Amacr"    #x0100) ; <ISOlat2> # LATIN CAPITAL LETTER A WITH MACRON
("amacr"    #x0101) ; <ISOlat2> # LATIN SMALL LETTER A WITH MACRON
("amalg"    #x2210) ; <ISOamsb> # N-ARY COPRODUCT
("amp"      #x0026) ; <ISOnum> # AMPERSAND
("and"      #x2227) ; <ISOtech> # LOGICAL AND
("ang"      #x2220) ; <ISOamso> # ANGLE
("ang90"    #x221F) ; <ISOtech> # RIGHT ANGLE
("angmsd"   #x2221) ; <ISOamso> # MEASURED ANGLE
("angsph"   #x2222) ; <ISOtech> # SPHERICAL ANGLE
("angst"    #x212B) ; <ISOtech> # ANGSTROM SIGN
("Aogon"    #x0104) ; <ISOlat2> # LATIN CAPITAL LETTER A WITH OGONEK
("aogon"    #x0105) ; <ISOlat2> # LATIN SMALL LETTER A WITH OGONEK
("ap"       #x2248) ; <ISOtech> # ALMOST EQUAL TO
("ape"      #x224A) ; <ISOamsr> # ALMOST EQUAL OR EQUAL TO
("apos"     #x02BC) ; <ISOnum> # MODIFIER LETTER APOSTROPHE
("Aring"    #x00C5) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH RING ABOVE
("aring"    #x00E5) ; <ISOlat1> # LATIN SMALL LETTER A WITH RING ABOVE
("ast"      #x002A) ; <ISOnum> # ASTERISK
("asymp"    #x2248) ; <ISOamsr> # ALMOST EQUAL TO
("Atilde"   #x00C3) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH TILDE
("atilde"   #x00E3) ; <ISOlat1> # LATIN SMALL LETTER A WITH TILDE
("Auml"     #x00C4) ; <ISOlat1> # LATIN CAPITAL LETTER A WITH DIAERESIS
("auml"     #x00E4) ; <ISOlat1> # LATIN SMALL LETTER A WITH DIAERESIS
("b.alpha"  #x03B1) ; <ISOgrk4> # GREEK SMALL LETTER ALPHA
("barwed"   #x22BC) ; <ISOamsb> # NAND
("Barwed"   #x2306) ; <ISOamsb> # PERSPECTIVE
("b.beta"   #x03B2) ; <ISOgrk4> # GREEK SMALL LETTER BETA
("b.chi"    #x03C7) ; <ISOgrk4> # GREEK SMALL LETTER CHI
("bcong"    #x224C) ; <ISOamsr> # ALL EQUAL TO
("Bcy"      #x0411) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER BE
("bcy"      #x0431) ; <ISOcyr1> # CYRILLIC SMALL LETTER BE
("b.Delta"  #x0394) ; <ISOgrk4> # GREEK CAPITAL LETTER DELTA
("b.delta"  #x03B4) ; <ISOgrk4> # GREEK SMALL LETTER DELTA
("bdquo"    #x201E) ; <HTMLspecial> # DOUBLE LOW-9 QUOTATION MARK
("becaus"   #x2235) ; <ISOtech> # BECAUSE
("bepsi"    #x220D) ; <ISOamsr> # SMALL CONTAINS AS MEMBER
("b.epsi"   #x03B5) ; <ISOgrk4> # GREEK SMALL LETTER EPSILON
("b.epsis"  #x03B5) ; <ISOgrk4> # GREEK SMALL LETTER EPSILON
("b.epsiv"  #x03B5) ; <ISOgrk4> # GREEK SMALL LETTER EPSILON
("bernou"   #x212C) ; <ISOtech> # SCRIPT CAPITAL B
("Beta"     #x0392) ; <HTMLsymbol> # GREEK CAPITAL LETTER BETA
("beta"     #x03B2) ; <ISOgrk3> # GREEK SMALL LETTER BETA
("b.eta"    #x03B7) ; <ISOgrk4> # GREEK SMALL LETTER ETA
("beth"     #x2136) ; <ISOamso> # BET SYMBOL
("b.Gamma"  #x0393) ; <ISOgrk4> # GREEK CAPITAL LETTER GAMMA
("b.gamma"  #x03B3) ; <ISOgrk4> # GREEK SMALL LETTER GAMMA
("b.gammad" #x03DC) ; <ISOgrk4> # GREEK LETTER DIGAMMA
("Bgr"      #x0392) ; <ISOgrk1> # GREEK CAPITAL LETTER BETA
("bgr"      #x03B2) ; <ISOgrk1> # GREEK SMALL LETTER BETA
("b.iota"   #x03B9) ; <ISOgrk4> # GREEK SMALL LETTER IOTA
("b.kappa"  #x03BA) ; <ISOgrk4> # GREEK SMALL LETTER KAPPA
("b.kappav" #x03F0) ; <ISOgrk4> # GREEK KAPPA SYMBOL
("b.Lambda" #x039B) ; <ISOgrk4> # GREEK CAPITAL LETTER LAMDA
("b.lambda" #x03BB) ; <ISOgrk4> # GREEK SMALL LETTER LAMDA
("blank"    #x2423) ; <ISOpub> # OPEN BOX
("blk12"    #x2592) ; <ISOpub> # MEDIUM SHADE
("blk14"    #x2591) ; <ISOpub> # LIGHT SHADE
("blk34"    #x2593) ; <ISOpub> # DARK SHADE
("block"    #x2588) ; <ISOpub> # FULL BLOCK
("b.mu"     #x03BC) ; <ISOgrk4> # GREEK SMALL LETTER MU
("b.nu"     #x03BD) ; <ISOgrk4> # GREEK SMALL LETTER NU
("b.Omega"  #x03A9) ; <ISOgrk4> # GREEK CAPITAL LETTER OMEGA
("b.omega"  #x03CE) ; <ISOgrk4> # GREEK SMALL LETTER OMEGA WITH TONOS
("bottom"   #x22A5) ; <ISOtech> # UP TACK
("bowtie"   #x22C8) ; <ISOamsr> # BOWTIE
("boxdl"    #x2510) ; <ISObox> # BOX DRAWINGS LIGHT DOWN AND LEFT
("boxdL"    #x2555) ; <ISObox> # BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
("boxDl"    #x2556) ; <ISObox> # BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
("boxDL"    #x2557) ; <ISObox> # BOX DRAWINGS DOUBLE DOWN AND LEFT
("boxdr"    #x250C) ; <ISObox> # BOX DRAWINGS LIGHT DOWN AND RIGHT
("boxdR"    #x2552) ; <ISObox> # BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
("boxDr"    #x2553) ; <ISObox> # BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
("boxDR"    #x2554) ; <ISObox> # BOX DRAWINGS DOUBLE DOWN AND RIGHT
("boxh"     #x2500) ; <ISObox> # BOX DRAWINGS LIGHT HORIZONTAL
("boxH"     #x2550) ; <ISObox> # BOX DRAWINGS DOUBLE HORIZONTAL
("boxhd"    #x252C) ; <ISObox> # BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
("boxHd"    #x2564) ; <ISObox> # BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
("boxhD"    #x2565) ; <ISObox> # BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
("boxHD"    #x2566) ; <ISObox> # BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
("boxhu"    #x2534) ; <ISObox> # BOX DRAWINGS LIGHT UP AND HORIZONTAL
("boxHu"    #x2567) ; <ISObox> # BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
("boxhU"    #x2568) ; <ISObox> # BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
("boxHU"    #x2569) ; <ISObox> # BOX DRAWINGS DOUBLE UP AND HORIZONTAL
("boxul"    #x2518) ; <ISObox> # BOX DRAWINGS LIGHT UP AND LEFT
("boxuL"    #x255B) ; <ISObox> # BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
("boxUl"    #x255C) ; <ISObox> # BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
("boxUL"    #x255D) ; <ISObox> # BOX DRAWINGS DOUBLE UP AND LEFT
("boxur"    #x2514) ; <ISObox> # BOX DRAWINGS LIGHT UP AND RIGHT
("boxuR"    #x2558) ; <ISObox> # BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
("boxUr"    #x2559) ; <ISObox> # BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
("boxUR"    #x255A) ; <ISObox> # BOX DRAWINGS DOUBLE UP AND RIGHT
("boxv"     #x2502) ; <ISObox> # BOX DRAWINGS LIGHT VERTICAL
("boxV"     #x2551) ; <ISObox> # BOX DRAWINGS DOUBLE VERTICAL
("boxvh"    #x253C) ; <ISObox> # BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
("boxvH"    #x256A) ; <ISObox> # BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
("boxVh"    #x256B) ; <ISObox> # BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
("boxVH"    #x256C) ; <ISObox> # BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
("boxvl"    #x2524) ; <ISObox> # BOX DRAWINGS LIGHT VERTICAL AND LEFT
("boxvL"    #x2561) ; <ISObox> # BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
("boxVl"    #x2562) ; <ISObox> # BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
("boxVL"    #x2563) ; <ISObox> # BOX DRAWINGS DOUBLE VERTICAL AND LEFT
("boxvr"    #x251C) ; <ISObox> # BOX DRAWINGS LIGHT VERTICAL AND RIGHT
("boxvR"    #x255E) ; <ISObox> # BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
("boxVr"    #x255F) ; <ISObox> # BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
("boxVR"    #x2560) ; <ISObox> # BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
("b.Phi"    #x03A6) ; <ISOgrk4> # GREEK CAPITAL LETTER PHI
("b.phis"   #x03C6) ; <ISOgrk4> # GREEK SMALL LETTER PHI
("b.phiv"   #x03D5) ; <ISOgrk4> # GREEK PHI SYMBOL
("b.Pi"     #x03A0) ; <ISOgrk4> # GREEK CAPITAL LETTER PI
("b.pi"     #x03C0) ; <ISOgrk4> # GREEK SMALL LETTER PI
("b.piv"    #x03D6) ; <ISOgrk4> # GREEK PI SYMBOL
("bprime"   #x2035) ; <ISOamso> # REVERSED PRIME
("b.Psi"    #x03A8) ; <ISOgrk4> # GREEK CAPITAL LETTER PSI
("b.psi"    #x03C8) ; <ISOgrk4> # GREEK SMALL LETTER PSI
("breve"    #x02D8) ; <ISOdia> # BREVE
("b.rho"    #x03C1) ; <ISOgrk4> # GREEK SMALL LETTER RHO
("b.rhov"   #x03F1) ; <ISOgrk4> # GREEK RHO SYMBOL
("brvbar"   #x00A6) ; <ISOnum> # BROKEN BAR
("b.Sigma"  #x03A3) ; <ISOgrk4> # GREEK CAPITAL LETTER SIGMA
("b.sigma"  #x03C3) ; <ISOgrk4> # GREEK SMALL LETTER SIGMA
("b.sigmav" #x03C2) ; <ISOgrk4> # GREEK SMALL LETTER FINAL SIGMA
("bsim"     #x223D) ; <ISOamsr> # REVERSED TILDE
("bsime"    #x22CD) ; <ISOamsr> # REVERSED TILDE EQUALS
("bsol"     #x005C) ; <ISOnum> # REVERSE SOLIDUS
("b.tau"    #x03C4) ; <ISOgrk4> # GREEK SMALL LETTER TAU
("b.Theta"  #x0398) ; <ISOgrk4> # GREEK CAPITAL LETTER THETA
("b.thetas" #x03B8) ; <ISOgrk4> # GREEK SMALL LETTER THETA
("b.thetav" #x03D1) ; <ISOgrk4> # GREEK THETA SYMBOL
("bull"     #x2022) ; <ISOpub> # BULLET
("bump"     #x224E) ; <ISOamsr> # GEOMETRICALLY EQUIVALENT TO
("bumpe"    #x224F) ; <ISOamsr> # DIFFERENCE BETWEEN
("b.Upsi"   #x03A5) ; <ISOgrk4> # GREEK CAPITAL LETTER UPSILON
("b.upsi"   #x03C5) ; <ISOgrk4> # GREEK SMALL LETTER UPSILON
("b.Xi"     #x039E) ; <ISOgrk4> # GREEK CAPITAL LETTER XI
("b.xi"     #x03BE) ; <ISOgrk4> # GREEK SMALL LETTER XI
("b.zeta"   #x03B6) ; <ISOgrk4> # GREEK SMALL LETTER ZETA
("Cacute"   #x0106) ; <ISOlat2> # LATIN CAPITAL LETTER C WITH ACUTE
("cacute"   #x0107) ; <ISOlat2> # LATIN SMALL LETTER C WITH ACUTE
("Cap"      #x22D2) ; <ISOamsb> # DOUBLE INTERSECTION
("cap"      #x2229) ; <ISOtech> # INTERSECTION
("caret"    #x2041) ; <ISOpub> # CARET INSERTION POINT
("caron"    #x02C7) ; <ISOdia> # CARON
("Ccaron"   #x010C) ; <ISOlat2> # LATIN CAPITAL LETTER C WITH CARON
("ccaron"   #x010D) ; <ISOlat2> # LATIN SMALL LETTER C WITH CARON
("Ccedil"   #x00C7) ; <ISOlat1> # LATIN CAPITAL LETTER C WITH CEDILLA
("ccedil"   #x00E7) ; <ISOlat1> # LATIN SMALL LETTER C WITH CEDILLA
("Ccirc"    #x0108) ; <ISOlat2> # LATIN CAPITAL LETTER C WITH CIRCUMFLEX
("ccirc"    #x0109) ; <ISOlat2> # LATIN SMALL LETTER C WITH CIRCUMFLEX
("Cdot"     #x010A) ; <ISOlat2> # LATIN CAPITAL LETTER C WITH DOT ABOVE
("cdot"     #x010B) ; <ISOlat2> # LATIN SMALL LETTER C WITH DOT ABOVE
("cedil"    #x00B8) ; <ISOdia> # CEDILLA
("cent"     #x00A2) ; <ISOnum> # CENT SIGN
("CHcy"     #x0427) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER CHE
("chcy"     #x0447) ; <ISOcyr1> # CYRILLIC SMALL LETTER CHE
("check"    #x2713) ; <ISOpub> # CHECK MARK
("Chi"      #x03A7) ; <HTMLsymbol> # GREEK CAPITAL LETTER CHI
("chi"      #x03C7) ; <ISOgrk3> # GREEK SMALL LETTER CHI
("cir"      #x25CB) ; <ISOpub> # WHITE CIRCLE
("circ"     #x02C6) ; <ISOdia> # MODIFIER LETTER CIRCUMFLEX ACCENT
("cire"     #x2257) ; <ISOamsr> # RING EQUAL TO
("clubs"    #x2663) ; <ISOpub> # BLACK CLUB SUIT
("colon"    #x003A) ; <ISOnum> # COLON
("colone"   #x2254) ; <ISOamsr> # COLON EQUALS
("comma"    #x002C) ; <ISOnum> # COMMA
("commat"   #x0040) ; <ISOnum> # COMMERCIAL AT
("comp"     #x2201) ; <ISOamso> # COMPLEMENT
("compfn"   #x2218) ; <ISOtech> # RING OPERATOR
("cong"     #x2245) ; <ISOtech> # APPROXIMATELY EQUAL TO
("conint"   #x222E) ; <ISOtech> # CONTOUR INTEGRAL
("coprod"   #x2210) ; <ISOamsb> # N-ARY COPRODUCT
("copy"     #x00A9) ; <ISOnum> # COPYRIGHT SIGN
("copysr"   #x2117) ; <ISOpub> # SOUND RECORDING COPYRIGHT
("crarr"    #x21B5) ; <HTMLsymbol> # DOWNWARDS ARROW WITH CORNER LEFTWARDS
("cross"    #x2717) ; <ISOpub> # BALLOT X
("cuepr"    #x22DE) ; <ISOamsr> # EQUAL TO OR PRECEDES
("cuesc"    #x22DF) ; <ISOamsr> # EQUAL TO OR SUCCEEDS
("cularr"   #x21B6) ; <ISOamsa> # ANTICLOCKWISE TOP SEMICIRCLE ARROW
("Cup"      #x22D3) ; <ISOamsb> # DOUBLE UNION
("cup"      #x222A) ; <ISOtech> # UNION
("cupre"    #x227C) ; <ISOamsr> # PRECEDES OR EQUAL TO
("curarr"   #x21B7) ; <ISOamsa> # CLOCKWISE TOP SEMICIRCLE ARROW
("curren"   #x00A4) ; <ISOnum> # CURRENCY SIGN
("cuvee"    #x22CE) ; <ISOamsb> # CURLY LOGICAL OR
("cuwed"    #x22CF) ; <ISOamsb> # CURLY LOGICAL AND
("dagger"   #x2020) ; <ISOpub> # DAGGER
("Dagger"   #x2021) ; <ISOpub> # DOUBLE DAGGER
("daleth"   #x2138) ; <ISOamso> # DALET SYMBOL
("dArr"     #x21D3) ; <ISOamsa> # DOWNWARDS DOUBLE ARROW
("darr"     #x2193) ; <ISOnum> # DOWNWARDS ARROW
("darr2"    #x21CA) ; <ISOamsa> # DOWNWARDS PAIRED ARROWS
("dash"     #x2010) ; <ISOpub> # HYPHEN
("dashv"    #x22A3) ; <ISOamsr> # LEFT TACK
("dblac"    #x02DD) ; <ISOdia> # DOUBLE ACUTE ACCENT
("Dcaron"   #x010E) ; <ISOlat2> # LATIN CAPITAL LETTER D WITH CARON
("dcaron"   #x010F) ; <ISOlat2> # LATIN SMALL LETTER D WITH CARON
("Dcy"      #x0414) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER DE
("dcy"      #x0434) ; <ISOcyr1> # CYRILLIC SMALL LETTER DE
("deg"      #x00B0) ; <ISOnum> # DEGREE SIGN
("Delta"    #x0394) ; <ISOgrk3> # GREEK CAPITAL LETTER DELTA
("delta"    #x03B4) ; <ISOgrk3> # GREEK SMALL LETTER DELTA
("Dgr"      #x0394) ; <ISOgrk1> # GREEK CAPITAL LETTER DELTA
("dgr"      #x03B4) ; <ISOgrk1> # GREEK SMALL LETTER DELTA
("dharl"    #x21C3) ; <ISOamsa> # DOWNWARDS HARPOON WITH BARB LEFTWARDS
("dharr"    #x21C2) ; <ISOamsa> # DOWNWARDS HARPOON WITH BARB RIGHTWARDS
("diam"     #x22C4) ; <ISOamsb> # DIAMOND OPERATOR
("diams"    #x2666) ; <ISOpub> # BLACK DIAMOND SUIT
("die"      #x00A8) ; <ISOdia> # DIAERESIS
("divide"   #x00F7) ; <ISOnum> # DIVISION SIGN
("divonx"   #x22C7) ; <ISOamsb> # DIVISION TIMES
("DJcy"     #x0402) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER DJE
("djcy"     #x0452) ; <ISOcyr2> # CYRILLIC SMALL LETTER DJE
("dlarr"    #x2199) ; <ISOamsa> # SOUTH WEST ARROW
("dlcorn"   #x231E) ; <ISOamsc> # BOTTOM LEFT CORNER
("dlcrop"   #x230D) ; <ISOpub> # BOTTOM LEFT CROP
("dollar"   #x0024) ; <ISOnum> # DOLLAR SIGN
("dot"      #x02D9) ; <ISOdia> # DOT ABOVE
("Dot"      #x00A8) ; <ISOtech> # DIAERESIS
("DotDot"   #x20DC) ; <ISOtech> # COMBINING FOUR DOTS ABOVE
("drarr"    #x2198) ; <ISOamsa> # SOUTH EAST ARROW
("drcorn"   #x231F) ; <ISOamsc> # BOTTOM RIGHT CORNER
("drcrop"   #x230C) ; <ISOpub> # BOTTOM RIGHT CROP
("DScy"     #x0405) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER DZE
("dscy"     #x0455) ; <ISOcyr2> # CYRILLIC SMALL LETTER DZE
("Dstrok"   #x0110) ; <ISOlat2> # LATIN CAPITAL LETTER D WITH STROKE
("dstrok"   #x0111) ; <ISOlat2> # LATIN SMALL LETTER D WITH STROKE
("dtri"     #x25BF) ; <ISOpub> # WHITE DOWN-POINTING SMALL TRIANGLE
("dtrif"    #x25BE) ; <ISOpub> # BLACK DOWN-POINTING SMALL TRIANGLE
("DZcy"     #x040F) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER DZHE
("dzcy"     #x045F) ; <ISOcyr2> # CYRILLIC SMALL LETTER DZHE
("Eacgr"    #x0388) ; <ISOgrk2> # GREEK CAPITAL LETTER EPSILON WITH TONOS
("eacgr"    #x03AD) ; <ISOgrk2> # GREEK SMALL LETTER EPSILON WITH TONOS
("Eacute"   #x00C9) ; <ISOlat1> # LATIN CAPITAL LETTER E WITH ACUTE
("eacute"   #x00E9) ; <ISOlat1> # LATIN SMALL LETTER E WITH ACUTE
("Ecaron"   #x011A) ; <ISOlat2> # LATIN CAPITAL LETTER E WITH CARON
("ecaron"   #x011B) ; <ISOlat2> # LATIN SMALL LETTER E WITH CARON
("ecir"     #x2256) ; <ISOamsr> # RING IN EQUAL TO
("Ecirc"    #x00CA) ; <ISOlat1> # LATIN CAPITAL LETTER E WITH CIRCUMFLEX
("ecirc"    #x00EA) ; <ISOlat1> # LATIN SMALL LETTER E WITH CIRCUMFLEX
("ecolon"   #x2255) ; <ISOamsr> # EQUALS COLON
("Ecy"      #x042D) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER E
("ecy"      #x044D) ; <ISOcyr1> # CYRILLIC SMALL LETTER E
("eDot"     #x2251) ; <ISOamsr> # GEOMETRICALLY EQUAL TO
("Edot"     #x0116) ; <ISOlat2> # LATIN CAPITAL LETTER E WITH DOT ABOVE
("edot"     #x0117) ; <ISOlat2> # LATIN SMALL LETTER E WITH DOT ABOVE
("EEacgr"   #x0389) ; <ISOgrk2> # GREEK CAPITAL LETTER ETA WITH TONOS
("eeacgr"   #x03AE) ; <ISOgrk2> # GREEK SMALL LETTER ETA WITH TONOS
("EEgr"     #x0397) ; <ISOgrk1> # GREEK CAPITAL LETTER ETA
("eegr"     #x03B7) ; <ISOgrk1> # GREEK SMALL LETTER ETA
("efDot"    #x2252) ; <ISOamsr> # APPROXIMATELY EQUAL TO OR THE IMAGE OF
("Egr"      #x0395) ; <ISOgrk1> # GREEK CAPITAL LETTER EPSILON
("egr"      #x03B5) ; <ISOgrk1> # GREEK SMALL LETTER EPSILON
("Egrave"   #x00C8) ; <ISOlat1> # LATIN CAPITAL LETTER E WITH GRAVE
("egrave"   #x00E8) ; <ISOlat1> # LATIN SMALL LETTER E WITH GRAVE
("egs"      #x22DD) ; <ISOamsr> # EQUAL TO OR GREATER-THAN
("ell"      #x2113) ; <ISOamso> # SCRIPT SMALL L
("els"      #x22DC) ; <ISOamsr> # EQUAL TO OR LESS-THAN
("Emacr"    #x0112) ; <ISOlat2> # LATIN CAPITAL LETTER E WITH MACRON
("emacr"    #x0113) ; <ISOlat2> # LATIN SMALL LETTER E WITH MACRON
("empty"    #x2205) ; <ISOamso> # EMPTY SET
("emsp"     #x2003) ; <ISOpub> # EM SPACE
("emsp13"   #x2004) ; <ISOpub> # THREE-PER-EM SPACE
("emsp14"   #x2005) ; <ISOpub> # FOUR-PER-EM SPACE
("ENG"      #x014A) ; <ISOlat2> # LATIN CAPITAL LETTER ENG
("eng"      #x014B) ; <ISOlat2> # LATIN SMALL LETTER ENG
("ensp"     #x2002) ; <ISOpub> # EN SPACE
("Eogon"    #x0118) ; <ISOlat2> # LATIN CAPITAL LETTER E WITH OGONEK
("eogon"    #x0119) ; <ISOlat2> # LATIN SMALL LETTER E WITH OGONEK
("epsi"     #x03B5) ; <ISOgrk3> # GREEK SMALL LETTER EPSILON
("Epsilon"  #x0395) ; <HTMLsymbol> # GREEK CAPITAL LETTER EPSILON
("epsilon"  #x03B5) ; <HTMLsymbol> # GREEK SMALL LETTER EPSILON
("epsis"    #x220A) ; <ISOgrk3> # SMALL ELEMENT OF
("equals"   #x003D) ; <ISOnum> # EQUALS SIGN
("equiv"    #x2261) ; <ISOtech> # IDENTICAL TO
("erDot"    #x2253) ; <ISOamsr> # IMAGE OF OR APPROXIMATELY EQUAL TO
("esdot"    #x2250) ; <ISOamsr> # APPROACHES THE LIMIT
("Eta"      #x0397) ; <HTMLsymbol> # GREEK CAPITAL LETTER ETA
("eta"      #x03B7) ; <ISOgrk3> # GREEK SMALL LETTER ETA
("ETH"      #x00D0) ; <ISOlat1> # LATIN CAPITAL LETTER ETH
("eth"      #x00F0) ; <ISOlat1> # LATIN SMALL LETTER ETH
("Euml"     #x00CB) ; <ISOlat1> # LATIN CAPITAL LETTER E WITH DIAERESIS
("euml"     #x00EB) ; <ISOlat1> # LATIN SMALL LETTER E WITH DIAERESIS
("excl"     #x0021) ; <ISOnum> # EXCLAMATION MARK
("exist"    #x2203) ; <ISOtech> # THERE EXISTS
("Fcy"      #x0424) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER EF
("fcy"      #x0444) ; <ISOcyr1> # CYRILLIC SMALL LETTER EF
("female"   #x2640) ; <ISOpub> # FEMALE SIGN
("ffilig"   #xFB03) ; <ISOpub> # LATIN SMALL LIGATURE FFI
("fflig"    #xFB00) ; <ISOpub> # LATIN SMALL LIGATURE FF
("ffllig"   #xFB04) ; <ISOpub> # LATIN SMALL LIGATURE FFL
("filig"    #xFB01) ; <ISOpub> # LATIN SMALL LIGATURE FI
("flat"     #x266D) ; <ISOpub> # MUSIC FLAT SIGN
("fllig"    #xFB02) ; <ISOpub> # LATIN SMALL LIGATURE FL
("fnof"     #x0192) ; <ISOtech> # LATIN SMALL LETTER F WITH HOOK
("forall"   #x2200) ; <ISOtech> # FOR ALL
("fork"     #x22D4) ; <ISOamsr> # PITCHFORK
("frac12"   #x00BD) ; <ISOnum> # VULGAR FRACTION ONE HALF
("frac13"   #x2153) ; <ISOpub> # VULGAR FRACTION ONE THIRD
("frac14"   #x00BC) ; <ISOnum> # VULGAR FRACTION ONE QUARTER
("frac15"   #x2155) ; <ISOpub> # VULGAR FRACTION ONE FIFTH
("frac16"   #x2159) ; <ISOpub> # VULGAR FRACTION ONE SIXTH
("frac18"   #x215B) ; <ISOnum> # VULGAR FRACTION ONE EIGHTH
("frac23"   #x2154) ; <ISOpub> # VULGAR FRACTION TWO THIRDS
("frac25"   #x2156) ; <ISOpub> # VULGAR FRACTION TWO FIFTHS
("frac34"   #x00BE) ; <ISOnum> # VULGAR FRACTION THREE QUARTERS
("frac35"   #x2157) ; <ISOpub> # VULGAR FRACTION THREE FIFTHS
("frac38"   #x215C) ; <ISOnum> # VULGAR FRACTION THREE EIGHTHS
("frac45"   #x2158) ; <ISOpub> # VULGAR FRACTION FOUR FIFTHS
("frac56"   #x215A) ; <ISOpub> # VULGAR FRACTION FIVE SIXTHS
("frac58"   #x215D) ; <ISOnum> # VULGAR FRACTION FIVE EIGHTHS
("frac78"   #x215E) ; <ISOnum> # VULGAR FRACTION SEVEN EIGHTHS
("frasl"    #x2044) ; <HTMLsymbol> # FRACTION SLASH
("frown"    #x2322) ; <ISOamsr> # FROWN
("gacute"   #x01F5) ; <ISOlat2> # LATIN SMALL LETTER G WITH ACUTE
("Gamma"    #x0393) ; <ISOgrk3> # GREEK CAPITAL LETTER GAMMA
("gamma"    #x03B3) ; <ISOgrk3> # GREEK SMALL LETTER GAMMA
("gammad"   #x03DC) ; <ISOgrk3> # GREEK LETTER DIGAMMA
("Gbreve"   #x011E) ; <ISOlat2> # LATIN CAPITAL LETTER G WITH BREVE
("gbreve"   #x011F) ; <ISOlat2> # LATIN SMALL LETTER G WITH BREVE
("Gcedil"   #x0122) ; <ISOlat2> # LATIN CAPITAL LETTER G WITH CEDILLA
("gcedil"   #x0123) ; <ISOlat2> # LATIN SMALL LETTER G WITH CEDILLA
("Gcirc"    #x011C) ; <ISOlat2> # LATIN CAPITAL LETTER G WITH CIRCUMFLEX
("gcirc"    #x011D) ; <ISOlat2> # LATIN SMALL LETTER G WITH CIRCUMFLEX
("Gcy"      #x0413) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER GHE
("gcy"      #x0433) ; <ISOcyr1> # CYRILLIC SMALL LETTER GHE
("Gdot"     #x0120) ; <ISOlat2> # LATIN CAPITAL LETTER G WITH DOT ABOVE
("gdot"     #x0121) ; <ISOlat2> # LATIN SMALL LETTER G WITH DOT ABOVE
("gE"       #x2267) ; <ISOamsr> # GREATER-THAN OVER EQUAL TO
("ge"       #x2265) ; <ISOtech> # GREATER-THAN OR EQUAL TO
("gel"      #x22DB) ; <ISOamsr> # GREATER-THAN EQUAL TO OR LESS-THAN
("ges"      #x2265) ; <ISOamsr> # GREATER-THAN OR EQUAL TO
("Gg"       #x22D9) ; <ISOamsr> # VERY MUCH GREATER-THAN
("Ggr"      #x0393) ; <ISOgrk1> # GREEK CAPITAL LETTER GAMMA
("ggr"      #x03B3) ; <ISOgrk1> # GREEK SMALL LETTER GAMMA
("gimel"    #x2137) ; <ISOamso> # GIMEL SYMBOL
("GJcy"     #x0403) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER GJE
("gjcy"     #x0453) ; <ISOcyr2> # CYRILLIC SMALL LETTER GJE
("gl"       #x2277) ; <ISOamsr> # GREATER-THAN OR LESS-THAN
("gne"      #x2269) ; <ISOamsn> # GREATER-THAN BUT NOT EQUAL TO
("gnE"      #x2269) ; <ISOamsn> # GREATER-THAN BUT NOT EQUAL TO
("gnsim"    #x22E7) ; <ISOamsn> # GREATER-THAN BUT NOT EQUIVALENT TO
("grave"    #x0060) ; <ISOdia> # GRAVE ACCENT
("gsdot"    #x22D7) ; <ISOamsr> # GREATER-THAN WITH DOT
("gsim"     #x2273) ; <ISOamsr> # GREATER-THAN OR EQUIVALENT TO
("Gt"       #x226B) ; <ISOamsr> # MUCH GREATER-THAN
("gt"       #x003E) ; <ISOnum> # GREATER-THAN SIGN
("gvnE"     #x2269) ; <ISOamsn> # GREATER-THAN BUT NOT EQUAL TO
("hairsp"   #x200A) ; <ISOpub> # HAIR SPACE
("half"     #x00BD) ; <ISOnum> # VULGAR FRACTION ONE HALF
("hamilt"   #x210B) ; <ISOtech> # SCRIPT CAPITAL H
("HARDcy"   #x042A) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER HARD SIGN
("hardcy"   #x044A) ; <ISOcyr1> # CYRILLIC SMALL LETTER HARD SIGN
("harr"     #x2194) ; <ISOamsa> # LEFT RIGHT ARROW
("hArr"     #x21D4) ; <ISOamsa> # LEFT RIGHT DOUBLE ARROW
("harrw"    #x21AD) ; <ISOamsa> # LEFT RIGHT WAVE ARROW
("Hcirc"    #x0124) ; <ISOlat2> # LATIN CAPITAL LETTER H WITH CIRCUMFLEX
("hcirc"    #x0125) ; <ISOlat2> # LATIN SMALL LETTER H WITH CIRCUMFLEX
("hearts"   #x2665) ; <ISOpub> # BLACK HEART SUIT
("hellip"   #x2026) ; <ISOpub> # HORIZONTAL ELLIPSIS
("horbar"   #x2015) ; <ISOnum> # HORIZONTAL BAR
("Hstrok"   #x0126) ; <ISOlat2> # LATIN CAPITAL LETTER H WITH STROKE
("hstrok"   #x0127) ; <ISOlat2> # LATIN SMALL LETTER H WITH STROKE
("hybull"   #x2043) ; <ISOpub> # HYPHEN BULLET
("hyphen"   #x002D) ; <ISOnum> # HYPHEN-MINUS
("Iacgr"    #x038A) ; <ISOgrk2> # GREEK CAPITAL LETTER IOTA WITH TONOS
("iacgr"    #x03AF) ; <ISOgrk2> # GREEK SMALL LETTER IOTA WITH TONOS
("Iacute"   #x00CD) ; <ISOlat1> # LATIN CAPITAL LETTER I WITH ACUTE
("iacute"   #x00ED) ; <ISOlat1> # LATIN SMALL LETTER I WITH ACUTE
("Icirc"    #x00CE) ; <ISOlat1> # LATIN CAPITAL LETTER I WITH CIRCUMFLEX
("icirc"    #x00EE) ; <ISOlat1> # LATIN SMALL LETTER I WITH CIRCUMFLEX
("Icy"      #x0418) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER I
("icy"      #x0438) ; <ISOcyr1> # CYRILLIC SMALL LETTER I
("idiagr"   #x0390) ; <ISOgrk2> # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
("Idigr"    #x03AA) ; <ISOgrk2> # GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
("idigr"    #x03CA) ; <ISOgrk2> # GREEK SMALL LETTER IOTA WITH DIALYTIKA
("Idot"     #x0130) ; <ISOlat2> # LATIN CAPITAL LETTER I WITH DOT ABOVE
("IEcy"     #x0415) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER IE
("iecy"     #x0435) ; <ISOcyr1> # CYRILLIC SMALL LETTER IE
("iexcl"    #x00A1) ; <ISOnum> # INVERTED EXCLAMATION MARK
("iff"      #x21D4) ; <ISOtech> # LEFT RIGHT DOUBLE ARROW
("Igr"      #x0399) ; <ISOgrk1> # GREEK CAPITAL LETTER IOTA
("igr"      #x03B9) ; <ISOgrk1> # GREEK SMALL LETTER IOTA
("Igrave"   #x00CC) ; <ISOlat1> # LATIN CAPITAL LETTER I WITH GRAVE
("igrave"   #x00EC) ; <ISOlat1> # LATIN SMALL LETTER I WITH GRAVE
("IJlig"    #x0132) ; <ISOlat2> # LATIN CAPITAL LIGATURE IJ
("ijlig"    #x0133) ; <ISOlat2> # LATIN SMALL LIGATURE IJ
("Imacr"    #x012A) ; <ISOlat2> # LATIN CAPITAL LETTER I WITH MACRON
("imacr"    #x012B) ; <ISOlat2> # LATIN SMALL LETTER I WITH MACRON
("image"    #x2111) ; <ISOamso> # BLACK-LETTER CAPITAL I
("incare"   #x2105) ; <ISOpub> # CARE OF
("infin"    #x221E) ; <ISOtech> # INFINITY
("inodot"   #x0131) ; <ISOamso> # LATIN SMALL LETTER DOTLESS I
("inodot"   #x0131) ; <ISOlat2> # LATIN SMALL LETTER DOTLESS I
("int"      #x222B) ; <ISOtech> # INTEGRAL
("intcal"   #x22BA) ; <ISOamsb> # INTERCALATE
("IOcy"     #x0401) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER IO
("iocy"     #x0451) ; <ISOcyr1> # CYRILLIC SMALL LETTER IO
("Iogon"    #x012E) ; <ISOlat2> # LATIN CAPITAL LETTER I WITH OGONEK
("iogon"    #x012F) ; <ISOlat2> # LATIN SMALL LETTER I WITH OGONEK
("Iota"     #x0399) ; <HTMLsymbol> # GREEK CAPITAL LETTER IOTA
("iota"     #x03B9) ; <ISOgrk3> # GREEK SMALL LETTER IOTA
("iquest"   #x00BF) ; <ISOnum> # INVERTED QUESTION MARK
("isin"     #x2208) ; <ISOtech> # ELEMENT OF
("Itilde"   #x0128) ; <ISOlat2> # LATIN CAPITAL LETTER I WITH TILDE
("itilde"   #x0129) ; <ISOlat2> # LATIN SMALL LETTER I WITH TILDE
("Iukcy"    #x0406) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
("iukcy"    #x0456) ; <ISOcyr2> # CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
("Iuml"     #x00CF) ; <ISOlat1> # LATIN CAPITAL LETTER I WITH DIAERESIS
("iuml"     #x00EF) ; <ISOlat1> # LATIN SMALL LETTER I WITH DIAERESIS
("Jcirc"    #x0134) ; <ISOlat2> # LATIN CAPITAL LETTER J WITH CIRCUMFLEX
("jcirc"    #x0135) ; <ISOlat2> # LATIN SMALL LETTER J WITH CIRCUMFLEX
("Jcy"      #x0419) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER SHORT I
("jcy"      #x0439) ; <ISOcyr1> # CYRILLIC SMALL LETTER SHORT I
("Jsercy"   #x0408) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER JE
("jsercy"   #x0458) ; <ISOcyr2> # CYRILLIC SMALL LETTER JE
("Jukcy"    #x0404) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER UKRAINIAN IE
("jukcy"    #x0454) ; <ISOcyr2> # CYRILLIC SMALL LETTER UKRAINIAN IE
("Kappa"    #x039A) ; <HTMLsymbol> # GREEK CAPITAL LETTER KAPPA
("kappa"    #x03BA) ; <ISOgrk3> # GREEK SMALL LETTER KAPPA
("kappav"   #x03F0) ; <ISOgrk3> # GREEK KAPPA SYMBOL
("Kcedil"   #x0136) ; <ISOlat2> # LATIN CAPITAL LETTER K WITH CEDILLA
("kcedil"   #x0137) ; <ISOlat2> # LATIN SMALL LETTER K WITH CEDILLA
("Kcy"      #x041A) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER KA
("kcy"      #x043A) ; <ISOcyr1> # CYRILLIC SMALL LETTER KA
("Kgr"      #x039A) ; <ISOgrk1> # GREEK CAPITAL LETTER KAPPA
("kgr"      #x03BA) ; <ISOgrk1> # GREEK SMALL LETTER KAPPA
("kgreen"   #x0138) ; <ISOlat2> # LATIN SMALL LETTER KRA
("KHcy"     #x0425) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER HA
("khcy"     #x0445) ; <ISOcyr1> # CYRILLIC SMALL LETTER HA
("KHgr"     #x03A7) ; <ISOgrk1> # GREEK CAPITAL LETTER CHI
("khgr"     #x03C7) ; <ISOgrk1> # GREEK SMALL LETTER CHI
("KJcy"     #x040C) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER KJE
("kjcy"     #x045C) ; <ISOcyr2> # CYRILLIC SMALL LETTER KJE
("lAarr"    #x21DA) ; <ISOamsa> # LEFTWARDS TRIPLE ARROW
("Lacute"   #x0139) ; <ISOlat2> # LATIN CAPITAL LETTER L WITH ACUTE
("lacute"   #x013A) ; <ISOlat2> # LATIN SMALL LETTER L WITH ACUTE
("lagran"   #x2112) ; <ISOtech> # SCRIPT CAPITAL L
("Lambda"   #x039B) ; <ISOgrk3> # GREEK CAPITAL LETTER LAMDA
("lambda"   #x03BB) ; <ISOgrk3> # GREEK SMALL LETTER LAMDA
("lang"     #x2329) ; <ISOtech> # LEFT-POINTING ANGLE BRACKET
("laquo"    #x00AB) ; <ISOnum> # LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
("Larr"     #x219E) ; <ISOamsa> # LEFTWARDS TWO HEADED ARROW
("larr"     #x2190) ; <ISOnum> # LEFTWARDS ARROW
("lArr"     #x21D0) ; <ISOtech> # LEFTWARDS DOUBLE ARROW
("larr2"    #x21C7) ; <ISOamsa> # LEFTWARDS PAIRED ARROWS
("larrhk"   #x21A9) ; <ISOamsa> # LEFTWARDS ARROW WITH HOOK
("larrlp"   #x21AB) ; <ISOamsa> # LEFTWARDS ARROW WITH LOOP
("larrtl"   #x21A2) ; <ISOamsa> # LEFTWARDS ARROW WITH TAIL
("Lcaron"   #x013D) ; <ISOlat2> # LATIN CAPITAL LETTER L WITH CARON
("lcaron"   #x013E) ; <ISOlat2> # LATIN SMALL LETTER L WITH CARON
("Lcedil"   #x013B) ; <ISOlat2> # LATIN CAPITAL LETTER L WITH CEDILLA
("lcedil"   #x013C) ; <ISOlat2> # LATIN SMALL LETTER L WITH CEDILLA
("lceil"    #x2308) ; <ISOamsc> # LEFT CEILING
("lcub"     #x007B) ; <ISOnum> # LEFT CURLY BRACKET
("Lcy"      #x041B) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER EL
("lcy"      #x043B) ; <ISOcyr1> # CYRILLIC SMALL LETTER EL
("ldot"     #x22D6) ; <ISOamsr> # LESS-THAN WITH DOT
("ldquo"    #x201C) ; <ISOnum> # LEFT DOUBLE QUOTATION MARK
("ldquor"   #x201E) ; <ISOpub> # DOUBLE LOW-9 QUOTATION MARK
("lE"       #x2266) ; <ISOamsr> # LESS-THAN OVER EQUAL TO
("le"       #x2264) ; <ISOtech> # LESS-THAN OR EQUAL TO
("leg"      #x22DA) ; <ISOamsr> # LESS-THAN EQUAL TO OR GREATER-THAN
("les"      #x2264) ; <ISOamsr> # LESS-THAN OR EQUAL TO
("lfloor"   #x230A) ; <ISOamsc> # LEFT FLOOR
("lg"       #x2276) ; <ISOamsr> # LESS-THAN OR GREATER-THAN
("Lgr"      #x039B) ; <ISOgrk1> # GREEK CAPITAL LETTER LAMDA
("lgr"      #x03BB) ; <ISOgrk1> # GREEK SMALL LETTER LAMDA
("lhard"    #x21BD) ; <ISOamsa> # LEFTWARDS HARPOON WITH BARB DOWNWARDS
("lharu"    #x21BC) ; <ISOamsa> # LEFTWARDS HARPOON WITH BARB UPWARDS
("lhblk"    #x2584) ; <ISOpub> # LOWER HALF BLOCK
("LJcy"     #x0409) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER LJE
("ljcy"     #x0459) ; <ISOcyr2> # CYRILLIC SMALL LETTER LJE
("Ll"       #x22D8) ; <ISOamsr> # VERY MUCH LESS-THAN
("Lmidot"   #x013F) ; <ISOlat2> # LATIN CAPITAL LETTER L WITH MIDDLE DOT
("lmidot"   #x0140) ; <ISOlat2> # LATIN SMALL LETTER L WITH MIDDLE DOT
("lnE"      #x2268) ; <ISOamsn> # LESS-THAN BUT NOT EQUAL TO
("lne"      #x2268) ; <ISOamsn> # LESS-THAN BUT NOT EQUAL TO
("lnsim"    #x22E6) ; <ISOamsn> # LESS-THAN BUT NOT EQUIVALENT TO
("lowast"   #x2217) ; <ISOtech> # ASTERISK OPERATOR
("lowbar"   #x005F) ; <ISOnum> # LOW LINE
("loz"      #x25CA) ; <ISOpub> # LOZENGE
("loz"      #x2727) ; <ISOpub> # WHITE FOUR POINTED STAR
("lozf"     #x2726) ; <ISOpub> # BLACK FOUR POINTED STAR
("lpar"     #x0028) ; <ISOnum> # LEFT PARENTHESIS
("lrarr2"   #x21C6) ; <ISOamsa> # LEFTWARDS ARROW OVER RIGHTWARDS ARROW
("lrhar2"   #x21CB) ; <ISOamsa> # LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
("lrm"      #x200E) ; <HTMLspecial> # LEFT-TO-RIGHT MARK
("lsaquo"   #x2039) ; <HTMLspecial> # SINGLE LEFT-POINTING ANGLE QUOTATION MARK
("lsh"      #x21B0) ; <ISOamsa> # UPWARDS ARROW WITH TIP LEFTWARDS
("lsim"     #x2272) ; <ISOamsr> # LESS-THAN OR EQUIVALENT TO
("lsqb"     #x005B) ; <ISOnum> # LEFT SQUARE BRACKET
("lsquo"    #x2018) ; <ISOnum> # LEFT SINGLE QUOTATION MARK
("lsquor"   #x201A) ; <ISOpub> # SINGLE LOW-9 QUOTATION MARK
("Lstrok"   #x0141) ; <ISOlat2> # LATIN CAPITAL LETTER L WITH STROKE
("lstrok"   #x0142) ; <ISOlat2> # LATIN SMALL LETTER L WITH STROKE
("Lt"       #x226A) ; <ISOamsr> # MUCH LESS-THAN
("lt"       #x003C) ; <ISOnum> # LESS-THAN SIGN
("lthree"   #x22CB) ; <ISOamsb> # LEFT SEMIDIRECT PRODUCT
("ltimes"   #x22C9) ; <ISOamsb> # LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
("ltri"     #x25C3) ; <ISOpub> # WHITE LEFT-POINTING SMALL TRIANGLE
("ltrie"    #x22B4) ; <ISOamsr> # NORMAL SUBGROUP OF OR EQUAL TO
("ltrif"    #x25C2) ; <ISOpub> # BLACK LEFT-POINTING SMALL TRIANGLE
("lvnE"     #x2268) ; <ISOamsn> # LESS-THAN BUT NOT EQUAL TO
("macr"     #x00AF) ; <ISOdia> # MACRON
("male"     #x2642) ; <ISOpub> # MALE SIGN
("malt"     #x2720) ; <ISOpub> # MALTESE CROSS
("map"      #x21A6) ; <ISOamsa> # RIGHTWARDS ARROW FROM BAR
("marker"   #x25AE) ; <ISOpub> # BLACK VERTICAL RECTANGLE
("Mcy"      #x041C) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER EM
("mcy"      #x043C) ; <ISOcyr1> # CYRILLIC SMALL LETTER EM
("mdash"    #x2014) ; <ISOpub> # EM DASH
("Mgr"      #x039C) ; <ISOgrk1> # GREEK CAPITAL LETTER MU
("mgr"      #x03BC) ; <ISOgrk1> # GREEK SMALL LETTER MU
("micro"    #x00B5) ; <ISOnum> # MICRO SIGN
("mid"      #x2223) ; <ISOamsr> # DIVIDES
("middot"   #x00B7) ; <ISOnum> # MIDDLE DOT
("minus"    #x2212) ; <ISOtech> # MINUS SIGN
("minusb"   #x229F) ; <ISOamsb> # SQUARED MINUS
("mldr"     #x2026) ; <ISOpub> # HORIZONTAL ELLIPSIS
("mnplus"   #x2213) ; <ISOtech> # MINUS-OR-PLUS SIGN
("models"   #x22A7) ; <ISOamsr> # MODELS
("Mu"       #x039C) ; <HTMLsymbol> # GREEK CAPITAL LETTER MU
("mu"       #x03BC) ; <ISOgrk3> # GREEK SMALL LETTER MU
("mumap"    #x22B8) ; <ISOamsa> # MULTIMAP
("nabla"    #x2207) ; <ISOtech> # NABLA
("Nacute"   #x0143) ; <ISOlat2> # LATIN CAPITAL LETTER N WITH ACUTE
("nacute"   #x0144) ; <ISOlat2> # LATIN SMALL LETTER N WITH ACUTE
("nap"      #x2249) ; <ISOamsn> # NOT ALMOST EQUAL TO
("napos"    #x0149) ; <ISOlat2> # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
("natur"    #x266E) ; <ISOpub> # MUSIC NATURAL SIGN
("nbsp"     #x00A0) ; <ISOnum> # NO-BREAK SPACE
("Ncaron"   #x0147) ; <ISOlat2> # LATIN CAPITAL LETTER N WITH CARON
("ncaron"   #x0148) ; <ISOlat2> # LATIN SMALL LETTER N WITH CARON
("Ncedil"   #x0145) ; <ISOlat2> # LATIN CAPITAL LETTER N WITH CEDILLA
("ncedil"   #x0146) ; <ISOlat2> # LATIN SMALL LETTER N WITH CEDILLA
("ncong"    #x2247) ; <ISOamsn> # NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
("Ncy"      #x041D) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER EN
("ncy"      #x043D) ; <ISOcyr1> # CYRILLIC SMALL LETTER EN
("ndash"    #x2013) ; <ISOpub> # EN DASH
("ne"       #x2260) ; <ISOtech> # NOT EQUAL TO
("nearr"    #x2197) ; <ISOamsa> # NORTH EAST ARROW
("nequiv"   #x2262) ; <ISOamsn> # NOT IDENTICAL TO
("nexist"   #x2204) ; <ISOamso> # THERE DOES NOT EXIST
("nge"      #x2271) ; <ISOamsn> # NEITHER GREATER-THAN NOR EQUAL TO
("nges"     #x2271) ; <ISOamsn> # NEITHER GREATER-THAN NOR EQUAL TO
("Ngr"      #x039D) ; <ISOgrk1> # GREEK CAPITAL LETTER NU
("ngr"      #x03BD) ; <ISOgrk1> # GREEK SMALL LETTER NU
("ngt"      #x226F) ; <ISOamsn> # NOT GREATER-THAN
("nharr"    #x21AE) ; <ISOamsa> # LEFT RIGHT ARROW WITH STROKE
("nhArr"    #x21CE) ; <ISOamsa> # LEFT RIGHT DOUBLE ARROW WITH STROKE
("ni"       #x220B) ; <ISOtech> # CONTAINS AS MEMBER
("NJcy"     #x040A) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER NJE
("njcy"     #x045A) ; <ISOcyr2> # CYRILLIC SMALL LETTER NJE
("nlarr"    #x219A) ; <ISOamsa> # LEFTWARDS ARROW WITH STROKE
("nlArr"    #x21CD) ; <ISOamsa> # LEFTWARDS DOUBLE ARROW WITH STROKE
("nldr"     #x2025) ; <ISOpub> # TWO DOT LEADER
("nle"      #x2270) ; <ISOamsn> # NEITHER LESS-THAN NOR EQUAL TO
("nles"     #x2270) ; <ISOamsn> # NEITHER LESS-THAN NOR EQUAL TO
("nlt"      #x226E) ; <ISOamsn> # NOT LESS-THAN
("nltri"    #x22EA) ; <ISOamsn> # NOT NORMAL SUBGROUP OF
("nltrie"   #x22EC) ; <ISOamsn> # NOT NORMAL SUBGROUP OF OR EQUAL TO
("nmid"     #x2224) ; <ISOamsn> # DOES NOT DIVIDE
("not"      #x00AC) ; <ISOnum> # NOT SIGN
("notin"    #x2209) ; <ISOtech> # NOT AN ELEMENT OF
("npar"     #x2226) ; <ISOamsn> # NOT PARALLEL TO
("npr"      #x2280) ; <ISOamsn> # DOES NOT PRECEDE
("npre"     #x22E0) ; <ISOamsn> # DOES NOT PRECEDE OR EQUAL
("nrarr"    #x219B) ; <ISOamsa> # RIGHTWARDS ARROW WITH STROKE
("nrArr"    #x21CF) ; <ISOamsa> # RIGHTWARDS DOUBLE ARROW WITH STROKE
("nrtri"    #x22EB) ; <ISOamsn> # DOES NOT CONTAIN AS NORMAL SUBGROUP
("nrtrie"   #x22ED) ; <ISOamsn> # DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
("nsc"      #x2281) ; <ISOamsn> # DOES NOT SUCCEED
("nsce"     #x22E1) ; <ISOamsn> # DOES NOT SUCCEED OR EQUAL
("nsim"     #x2241) ; <ISOamsn> # NOT TILDE
("nsime"    #x2244) ; <ISOamsn> # NOT ASYMPTOTICALLY EQUAL TO
("nspar"    #x2226) ; <ISOamsn> # NOT PARALLEL TO
("nsub"     #x2284) ; <ISOamsn> # NOT A SUBSET OF
("nsubE"    #x2288) ; <ISOamsn> # NEITHER A SUBSET OF NOR EQUAL TO
("nsube"    #x2288) ; <ISOamsn> # NEITHER A SUBSET OF NOR EQUAL TO
("nsup"     #x2285) ; <ISOamsn> # NOT A SUPERSET OF
("nsupE"    #x2289) ; <ISOamsn> # NEITHER A SUPERSET OF NOR EQUAL TO
("nsupe"    #x2289) ; <ISOamsn> # NEITHER A SUPERSET OF NOR EQUAL TO
("Ntilde"   #x00D1) ; <ISOlat1> # LATIN CAPITAL LETTER N WITH TILDE
("ntilde"   #x00F1) ; <ISOlat1> # LATIN SMALL LETTER N WITH TILDE
("Nu"       #x039D) ; <HTMLsymbol> # GREEK CAPITAL LETTER NU
("nu"       #x03BD) ; <ISOgrk3> # GREEK SMALL LETTER NU
("num"      #x0023) ; <ISOnum> # NUMBER SIGN
("numero"   #x2116) ; <ISOcyr1> # NUMERO SIGN
("numsp"    #x2007) ; <ISOpub> # FIGURE SPACE
("nvdash"   #x22AC) ; <ISOamsn> # DOES NOT PROVE
("nvDash"   #x22AD) ; <ISOamsn> # NOT TRUE
("nVdash"   #x22AE) ; <ISOamsn> # DOES NOT FORCE
("nVDash"   #x22AF) ; <ISOamsn> # NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
("nwarr"    #x2196) ; <ISOamsa> # NORTH WEST ARROW
("Oacgr"    #x038C) ; <ISOgrk2> # GREEK CAPITAL LETTER OMICRON WITH TONOS
("oacgr"    #x03CC) ; <ISOgrk2> # GREEK SMALL LETTER OMICRON WITH TONOS
("Oacute"   #x00D3) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH ACUTE
("oacute"   #x00F3) ; <ISOlat1> # LATIN SMALL LETTER O WITH ACUTE
("oast"     #x229B) ; <ISOamsb> # CIRCLED ASTERISK OPERATOR
("ocir"     #x229A) ; <ISOamsb> # CIRCLED RING OPERATOR
("Ocirc"    #x00D4) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH CIRCUMFLEX
("ocirc"    #x00F4) ; <ISOlat1> # LATIN SMALL LETTER O WITH CIRCUMFLEX
("Ocy"      #x041E) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER O
("ocy"      #x043E) ; <ISOcyr1> # CYRILLIC SMALL LETTER O
("odash"    #x229D) ; <ISOamsb> # CIRCLED DASH
("Odblac"   #x0150) ; <ISOlat2> # LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
("odblac"   #x0151) ; <ISOlat2> # LATIN SMALL LETTER O WITH DOUBLE ACUTE
("odot"     #x2299) ; <ISOamsb> # CIRCLED DOT OPERATOR
("OElig"    #x0152) ; <ISOlat2> # LATIN CAPITAL LIGATURE OE
("oelig"    #x0153) ; <ISOlat2> # LATIN SMALL LIGATURE OE
("ogon"     #x02DB) ; <ISOdia> # OGONEK
("Ogr"      #x039F) ; <ISOgrk1> # GREEK CAPITAL LETTER OMICRON
("ogr"      #x03BF) ; <ISOgrk1> # GREEK SMALL LETTER OMICRON
("Ograve"   #x00D2) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH GRAVE
("ograve"   #x00F2) ; <ISOlat1> # LATIN SMALL LETTER O WITH GRAVE
("OHacgr"   #x038F) ; <ISOgrk2> # GREEK CAPITAL LETTER OMEGA WITH TONOS
("ohacgr"   #x03CE) ; <ISOgrk2> # GREEK SMALL LETTER OMEGA WITH TONOS
("OHgr"     #x03A9) ; <ISOgrk1> # GREEK CAPITAL LETTER OMEGA
("ohgr"     #x03C9) ; <ISOgrk1> # GREEK SMALL LETTER OMEGA
("ohm"      #x2126) ; <ISOnum> # OHM SIGN
("olarr"    #x21BA) ; <ISOamsa> # ANTICLOCKWISE OPEN CIRCLE ARROW
("oline"    #x203E) ; <HTMLsymbol> # OVERLINE
("Omacr"    #x014C) ; <ISOlat2> # LATIN CAPITAL LETTER O WITH MACRON
("omacr"    #x014D) ; <ISOlat2> # LATIN SMALL LETTER O WITH MACRON
("Omega"    #x03A9) ; <ISOgrk3> # GREEK CAPITAL LETTER OMEGA
("omega"    #x03C9) ; <ISOgrk3> # GREEK SMALL LETTER OMEGA
("Omicron"  #x039F) ; <HTMLsymbol> # GREEK CAPITAL LETTER OMICRON
("omicron"  #x03BF) ; <HTMLsymbol> # GREEK SMALL LETTER OMICRON
("ominus"   #x2296) ; <ISOamsb> # CIRCLED MINUS
("oplus"    #x2295) ; <ISOamsb> # CIRCLED PLUS
("or"       #x2228) ; <ISOtech> # LOGICAL OR
("orarr"    #x21BB) ; <ISOamsa> # CLOCKWISE OPEN CIRCLE ARROW
("order"    #x2134) ; <ISOtech> # SCRIPT SMALL O
("ordf"     #x00AA) ; <ISOnum> # FEMININE ORDINAL INDICATOR
("ordm"     #x00BA) ; <ISOnum> # MASCULINE ORDINAL INDICATOR
("oS"       #x24C8) ; <ISOamso> # CIRCLED LATIN CAPITAL LETTER S
("Oslash"   #x00D8) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH STROKE
("oslash"   #x00F8) ; <ISOlat1> # LATIN SMALL LETTER O WITH STROKE
("osol"     #x2298) ; <ISOamsb> # CIRCLED DIVISION SLASH
("Otilde"   #x00D5) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH TILDE
("otilde"   #x00F5) ; <ISOlat1> # LATIN SMALL LETTER O WITH TILDE
("otimes"   #x2297) ; <ISOamsb> # CIRCLED TIMES
("Ouml"     #x00D6) ; <ISOlat1> # LATIN CAPITAL LETTER O WITH DIAERESIS
("ouml"     #x00F6) ; <ISOlat1> # LATIN SMALL LETTER O WITH DIAERESIS
("par"      #x2225) ; <ISOtech> # PARALLEL TO
("para"     #x00B6) ; <ISOnum> # PILCROW SIGN
("part"     #x2202) ; <ISOtech> # PARTIAL DIFFERENTIAL
("Pcy"      #x041F) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER PE
("pcy"      #x043F) ; <ISOcyr1> # CYRILLIC SMALL LETTER PE
("percnt"   #x0025) ; <ISOnum> # PERCENT SIGN
("period"   #x002E) ; <ISOnum> # FULL STOP
("permil"   #x2030) ; <ISOtech> # PER MILLE SIGN
("perp"     #x22A5) ; <ISOtech> # UP TACK
("Pgr"      #x03A0) ; <ISOgrk1> # GREEK CAPITAL LETTER PI
("pgr"      #x03C0) ; <ISOgrk1> # GREEK SMALL LETTER PI
("PHgr"     #x03A6) ; <ISOgrk1> # GREEK CAPITAL LETTER PHI
("phgr"     #x03C6) ; <ISOgrk1> # GREEK SMALL LETTER PHI
("phi"      #x03C6) ; <HTMLsymbol> # GREEK SMALL LETTER PHI
("Phi"      #x03A6) ; <ISOgrk3> # GREEK CAPITAL LETTER PHI
("phis"     #x03C6) ; <ISOgrk3> # GREEK SMALL LETTER PHI
("phiv"     #x03D5) ; <ISOgrk3> # GREEK PHI SYMBOL
("phmmat"   #x2133) ; <ISOtech> # SCRIPT CAPITAL M
("phone"    #x260E) ; <ISOpub> # BLACK TELEPHONE
("Pi"       #x03A0) ; <ISOgrk3> # GREEK CAPITAL LETTER PI
("pi"       #x03C0) ; <ISOgrk3> # GREEK SMALL LETTER PI
("piv"      #x03D6) ; <ISOgrk3> # GREEK PI SYMBOL
("planck"   #x210F) ; <ISOamso> # PLANCK CONSTANT OVER TWO PI
("plus"     #x002B) ; <ISOnum> # PLUS SIGN
("plusb"    #x229E) ; <ISOamsb> # SQUARED PLUS
("plusdo"   #x2214) ; <ISOamsb> # DOT PLUS
("plusmn"   #x00B1) ; <ISOnum> # PLUS-MINUS SIGN
("pound"    #x00A3) ; <ISOnum> # POUND SIGN
("pr"       #x227A) ; <ISOamsr> # PRECEDES
("pre"      #x227C) ; <ISOamsr> # PRECEDES OR EQUAL TO
("prime"    #x2032) ; <ISOtech> # PRIME
("Prime"    #x2033) ; <ISOtech> # DOUBLE PRIME
("prnsim"   #x22E8) ; <ISOamsn> # PRECEDES BUT NOT EQUIVALENT TO
("prod"     #x220F) ; <ISOamsb> # N-ARY PRODUCT
("prop"     #x221D) ; <ISOtech> # PROPORTIONAL TO
("prsim"    #x227E) ; <ISOamsr> # PRECEDES OR EQUIVALENT TO
("PSgr"     #x03A8) ; <ISOgrk1> # GREEK CAPITAL LETTER PSI
("psgr"     #x03C8) ; <ISOgrk1> # GREEK SMALL LETTER PSI
("Psi"      #x03A8) ; <ISOgrk3> # GREEK CAPITAL LETTER PSI
("psi"      #x03C8) ; <ISOgrk3> # GREEK SMALL LETTER PSI
("puncsp"   #x2008) ; <ISOpub> # PUNCTUATION SPACE
("quest"    #x003F) ; <ISOnum> # QUESTION MARK
("quot"     #x0022) ; <ISOnum> # QUOTATION MARK
("rAarr"    #x21DB) ; <ISOamsa> # RIGHTWARDS TRIPLE ARROW
("Racute"   #x0154) ; <ISOlat2> # LATIN CAPITAL LETTER R WITH ACUTE
("racute"   #x0155) ; <ISOlat2> # LATIN SMALL LETTER R WITH ACUTE
("radic"    #x221A) ; <ISOtech> # SQUARE ROOT
("rang"     #x232A) ; <ISOtech> # RIGHT-POINTING ANGLE BRACKET
("raquo"    #x00BB) ; <ISOnum> # RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
("Rarr"     #x21A0) ; <ISOamsa> # RIGHTWARDS TWO HEADED ARROW
("rarr"     #x2192) ; <ISOnum> # RIGHTWARDS ARROW
("rArr"     #x21D2) ; <ISOtech> # RIGHTWARDS DOUBLE ARROW
("rarr2"    #x21C9) ; <ISOamsa> # RIGHTWARDS PAIRED ARROWS
("rarrhk"   #x21AA) ; <ISOamsa> # RIGHTWARDS ARROW WITH HOOK
("rarrlp"   #x21AC) ; <ISOamsa> # RIGHTWARDS ARROW WITH LOOP
("rarrtl"   #x21A3) ; <ISOamsa> # RIGHTWARDS ARROW WITH TAIL
("rarrw"    #x219D) ; <ISOamsa> # RIGHTWARDS WAVE ARROW
("Rcaron"   #x0158) ; <ISOlat2> # LATIN CAPITAL LETTER R WITH CARON
("rcaron"   #x0159) ; <ISOlat2> # LATIN SMALL LETTER R WITH CARON
("Rcedil"   #x0156) ; <ISOlat2> # LATIN CAPITAL LETTER R WITH CEDILLA
("rcedil"   #x0157) ; <ISOlat2> # LATIN SMALL LETTER R WITH CEDILLA
("rceil"    #x2309) ; <ISOamsc> # RIGHT CEILING
("rcub"     #x007D) ; <ISOnum> # RIGHT CURLY BRACKET
("Rcy"      #x0420) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER ER
("rcy"      #x0440) ; <ISOcyr1> # CYRILLIC SMALL LETTER ER
("rdquo"    #x201D) ; <ISOnum> # RIGHT DOUBLE QUOTATION MARK
("rdquor"   #x201C) ; <ISOpub> # LEFT DOUBLE QUOTATION MARK
("real"     #x211C) ; <ISOamso> # BLACK-LETTER CAPITAL R
("rect"     #x25AD) ; <ISOpub> # WHITE RECTANGLE
("reg"      #x00AE) ; <ISOnum> # REGISTERED SIGN
("rfloor"   #x230B) ; <ISOamsc> # RIGHT FLOOR
("Rgr"      #x03A1) ; <ISOgrk1> # GREEK CAPITAL LETTER RHO
("rgr"      #x03C1) ; <ISOgrk1> # GREEK SMALL LETTER RHO
("rhard"    #x21C1) ; <ISOamsa> # RIGHTWARDS HARPOON WITH BARB DOWNWARDS
("rharu"    #x21C0) ; <ISOamsa> # RIGHTWARDS HARPOON WITH BARB UPWARDS
("Rho"      #x03A1) ; <HTMLsymbol> # GREEK CAPITAL LETTER RHO
("rho"      #x03C1) ; <ISOgrk3> # GREEK SMALL LETTER RHO
("rhov"     #x03F1) ; <ISOgrk3> # GREEK RHO SYMBOL
("ring"     #x02DA) ; <ISOdia> # RING ABOVE
("rlarr2"   #x21C4) ; <ISOamsa> # RIGHTWARDS ARROW OVER LEFTWARDS ARROW
("rlhar2"   #x21CC) ; <ISOamsa> # RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
("rlm"      #x200F) ; <HTMLspecial> # RIGHT-TO-LEFT MARK
("rpar"     #x0029) ; <ISOnum> # RIGHT PARENTHESIS
("rsaquo"   #x203A) ; <HTMLspecial> # SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
("rsh"      #x21B1) ; <ISOamsa> # UPWARDS ARROW WITH TIP RIGHTWARDS
("rsqb"     #x005D) ; <ISOnum> # RIGHT SQUARE BRACKET
("rsquo"    #x2019) ; <ISOnum> # RIGHT SINGLE QUOTATION MARK
("rsquor"   #x2018) ; <ISOpub> # LEFT SINGLE QUOTATION MARK
("rthree"   #x22CC) ; <ISOamsb> # RIGHT SEMIDIRECT PRODUCT
("rtimes"   #x22CA) ; <ISOamsb> # RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
("rtri"     #x25B9) ; <ISOpub> # WHITE RIGHT-POINTING SMALL TRIANGLE
("rtrie"    #x22B5) ; <ISOamsr> # CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
("rtrif"    #x25B8) ; <ISOpub> # BLACK RIGHT-POINTING SMALL TRIANGLE
("rx"       #x211E) ; <ISOpub> # PRESCRIPTION TAKE
("Sacute"   #x015A) ; <ISOlat2> # LATIN CAPITAL LETTER S WITH ACUTE
("sacute"   #x015B) ; <ISOlat2> # LATIN SMALL LETTER S WITH ACUTE
("samalg"   #x2210) ; <ISOamsr> # N-ARY COPRODUCT
("sbquo"    #x201A) ; <HTMLspecial> # SINGLE LOW-9 QUOTATION MARK
("sbsol"    #x005C) ; <ISOamso> # REVERSE SOLIDUS
("sc"       #x227B) ; <ISOamsr> # SUCCEEDS
("Scaron"   #x0160) ; <ISOlat2> # LATIN CAPITAL LETTER S WITH CARON
("scaron"   #x0161) ; <ISOlat2> # LATIN SMALL LETTER S WITH CARON
("sccue"    #x227D) ; <ISOamsr> # SUCCEEDS OR EQUAL TO
("sce"      #x227D) ; <ISOamsr> # SUCCEEDS OR EQUAL TO
("Scedil"   #x015E) ; <ISOlat2> # LATIN CAPITAL LETTER S WITH CEDILLA
("scedil"   #x015F) ; <ISOlat2> # LATIN SMALL LETTER S WITH CEDILLA
("Scirc"    #x015C) ; <ISOlat2> # LATIN CAPITAL LETTER S WITH CIRCUMFLEX
("scirc"    #x015D) ; <ISOlat2> # LATIN SMALL LETTER S WITH CIRCUMFLEX
("scnsim"   #x22E9) ; <ISOamsn> # SUCCEEDS BUT NOT EQUIVALENT TO
("scsim"    #x227F) ; <ISOamsr> # SUCCEEDS OR EQUIVALENT TO
("Scy"      #x0421) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER ES
("scy"      #x0441) ; <ISOcyr1> # CYRILLIC SMALL LETTER ES
("sdot"     #x22C5) ; <ISOamsb> # DOT OPERATOR
("sdotb"    #x22A1) ; <ISOamsb> # SQUARED DOT OPERATOR
("sect"     #x00A7) ; <ISOnum> # SECTION SIGN
("semi"     #x003B) ; <ISOnum> # SEMICOLON
("setmn"    #x2216) ; <ISOamsb> # SET MINUS
("sext"     #x2736) ; <ISOpub> # SIX POINTED BLACK STAR
("sfgr"     #x03C2) ; <ISOgrk1> # GREEK SMALL LETTER FINAL SIGMA
("sfrown"   #x2322) ; <ISOamsr> # FROWN
("Sgr"      #x03A3) ; <ISOgrk1> # GREEK CAPITAL LETTER SIGMA
("sgr"      #x03C3) ; <ISOgrk1> # GREEK SMALL LETTER SIGMA
("sharp"    #x266F) ; <ISOpub> # MUSIC SHARP SIGN
("SHCHcy"   #x0429) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER SHCHA
("shchcy"   #x0449) ; <ISOcyr1> # CYRILLIC SMALL LETTER SHCHA
("SHcy"     #x0428) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER SHA
("shcy"     #x0448) ; <ISOcyr1> # CYRILLIC SMALL LETTER SHA
("shy"      #x00AD) ; <ISOnum> # SOFT HYPHEN
("Sigma"    #x03A3) ; <ISOgrk3> # GREEK CAPITAL LETTER SIGMA
("sigma"    #x03C3) ; <ISOgrk3> # GREEK SMALL LETTER SIGMA
("sigmaf"   #x03C2) ; <HTMLsymbol> # GREEK SMALL LETTER FINAL SIGMA
("sigmav"   #x03C2) ; <ISOgrk3> # GREEK SMALL LETTER FINAL SIGMA
("sim"      #x223C) ; <ISOtech> # TILDE OPERATOR
("sime"     #x2243) ; <ISOtech> # ASYMPTOTICALLY EQUAL TO
("smile"    #x2323) ; <ISOamsr> # SMILE
("SOFTcy"   #x042C) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER SOFT SIGN
("softcy"   #x044C) ; <ISOcyr1> # CYRILLIC SMALL LETTER SOFT SIGN
("sol"      #x002F) ; <ISOnum> # SOLIDUS
("spades"   #x2660) ; <ISOpub> # BLACK SPADE SUIT
("spar"     #x2225) ; <ISOamsr> # PARALLEL TO
("sqcap"    #x2293) ; <ISOamsb> # SQUARE CAP
("sqcup"    #x2294) ; <ISOamsb> # SQUARE CUP
("sqsub"    #x228F) ; <ISOamsr> # SQUARE IMAGE OF
("sqsube"   #x2291) ; <ISOamsr> # SQUARE IMAGE OF OR EQUAL TO
("sqsup"    #x2290) ; <ISOamsr> # SQUARE ORIGINAL OF
("sqsupe"   #x2292) ; <ISOamsr> # SQUARE ORIGINAL OF OR EQUAL TO
("squ"      #x25A1) ; <ISOpub> # WHITE SQUARE
("square"   #x25A1) ; <ISOtech> # WHITE SQUARE
("squf"     #x25AA) ; <ISOpub> # BLACK SMALL SQUARE
("ssetmn"   #x2216) ; <ISOamsb> # SET MINUS
("ssmile"   #x2323) ; <ISOamsr> # SMILE
("sstarf"   #x22C6) ; <ISOamsb> # STAR OPERATOR
("star"     #x2606) ; <ISOpub> # WHITE STAR
("starf"    #x2605) ; <ISOpub> # BLACK STAR
("Sub"      #x22D0) ; <ISOamsr> # DOUBLE SUBSET
("sub"      #x2282) ; <ISOtech> # SUBSET OF
("subE"     #x2286) ; <ISOamsr> # SUBSET OF OR EQUAL TO
("sube"     #x2286) ; <ISOtech> # SUBSET OF OR EQUAL TO
("subnE"    #x228A) ; <ISOamsn> # SUBSET OF WITH NOT EQUAL TO
("subne"    #x228A) ; <ISOamsn> # SUBSET OF WITH NOT EQUAL TO
("sum"      #x2211) ; <ISOamsb> # N-ARY SUMMATION
("sung"     #x266A) ; <ISOnum> # EIGHTH NOTE
("Sup"      #x22D1) ; <ISOamsr> # DOUBLE SUPERSET
("sup"      #x2283) ; <ISOtech> # SUPERSET OF
("sup1"     #x00B9) ; <ISOnum> # SUPERSCRIPT ONE
("sup2"     #x00B2) ; <ISOnum> # SUPERSCRIPT TWO
("sup3"     #x00B3) ; <ISOnum> # SUPERSCRIPT THREE
("supE"     #x2287) ; <ISOamsr> # SUPERSET OF OR EQUAL TO
("supe"     #x2287) ; <ISOtech> # SUPERSET OF OR EQUAL TO
("supnE"    #x228B) ; <ISOamsn> # SUPERSET OF WITH NOT EQUAL TO
("supne"    #x228B) ; <ISOamsn> # SUPERSET OF WITH NOT EQUAL TO
("szlig"    #x00DF) ; <ISOlat1> # LATIN SMALL LETTER SHARP S
("target"   #x2316) ; <ISOpub> # POSITION INDICATOR
("Tau"      #x03A4) ; <HTMLsymbol> # GREEK CAPITAL LETTER TAU
("tau"      #x03C4) ; <ISOgrk3> # GREEK SMALL LETTER TAU
("Tcaron"   #x0164) ; <ISOlat2> # LATIN CAPITAL LETTER T WITH CARON
("tcaron"   #x0165) ; <ISOlat2> # LATIN SMALL LETTER T WITH CARON
("Tcedil"   #x0162) ; <ISOlat2> # LATIN CAPITAL LETTER T WITH CEDILLA
("tcedil"   #x0163) ; <ISOlat2> # LATIN SMALL LETTER T WITH CEDILLA
("Tcy"      #x0422) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER TE
("tcy"      #x0442) ; <ISOcyr1> # CYRILLIC SMALL LETTER TE
("tdot"     #x20DB) ; <ISOtech> # COMBINING THREE DOTS ABOVE
("telrec"   #x2315) ; <ISOpub> # TELEPHONE RECORDER
("Tgr"      #x03A4) ; <ISOgrk1> # GREEK CAPITAL LETTER TAU
("tgr"      #x03C4) ; <ISOgrk1> # GREEK SMALL LETTER TAU
("there4"   #x2234) ; <ISOtech> # THEREFORE
("theta"    #x03B8) ; <HTMLsymbol> # GREEK SMALL LETTER THETA
("Theta"    #x0398) ; <ISOgrk3> # GREEK CAPITAL LETTER THETA
("thetas"   #x03B8) ; <ISOgrk3> # GREEK SMALL LETTER THETA
("thetasym" #x03D1) ; <HTMLsymbol> # GREEK THETA SYMBOL
("thetav"   #x03D1) ; <ISOgrk3> # GREEK THETA SYMBOL
("THgr"     #x0398) ; <ISOgrk1> # GREEK CAPITAL LETTER THETA
("thgr"     #x03B8) ; <ISOgrk1> # GREEK SMALL LETTER THETA
("thinsp"   #x2009) ; <ISOpub> # THIN SPACE
("thkap"    #x2248) ; <ISOamsr> # ALMOST EQUAL TO
("thksim"   #x223C) ; <ISOamsr> # TILDE OPERATOR
("THORN"    #x00DE) ; <ISOlat1> # LATIN CAPITAL LETTER THORN
("thorn"    #x00FE) ; <ISOlat1> # LATIN SMALL LETTER THORN
("tilde"    #x02DC) ; <ISOdia> # SMALL TILDE
("times"    #x00D7) ; <ISOnum> # MULTIPLICATION SIGN
("timesb"   #x22A0) ; <ISOamsb> # SQUARED TIMES
("top"      #x22A4) ; <ISOamsb> # DOWN TACK
("tprime"   #x2034) ; <ISOtech> # TRIPLE PRIME
("trade"    #x2122) ; <ISOnum> # TRADE MARK SIGN
("trie"     #x225C) ; <ISOamsr> # DELTA EQUAL TO
("TScy"     #x0426) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER TSE
("tscy"     #x0446) ; <ISOcyr1> # CYRILLIC SMALL LETTER TSE
("TSHcy"    #x040B) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER TSHE
("tshcy"    #x045B) ; <ISOcyr2> # CYRILLIC SMALL LETTER TSHE
("Tstrok"   #x0166) ; <ISOlat2> # LATIN CAPITAL LETTER T WITH STROKE
("tstrok"   #x0167) ; <ISOlat2> # LATIN SMALL LETTER T WITH STROKE
("twixt"    #x226C) ; <ISOamsr> # BETWEEN
("Uacgr"    #x038E) ; <ISOgrk2> # GREEK CAPITAL LETTER UPSILON WITH TONOS
("uacgr"    #x03CD) ; <ISOgrk2> # GREEK SMALL LETTER UPSILON WITH TONOS
("Uacute"   #x00DA) ; <ISOlat1> # LATIN CAPITAL LETTER U WITH ACUTE
("uacute"   #x00FA) ; <ISOlat1> # LATIN SMALL LETTER U WITH ACUTE
("uArr"     #x21D1) ; <ISOamsa> # UPWARDS DOUBLE ARROW
("uarr"     #x2191) ; <ISOnum> # UPWARDS ARROW
("uarr2"    #x21C8) ; <ISOamsa> # UPWARDS PAIRED ARROWS
("Ubrcy"    #x040E) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER SHORT U
("ubrcy"    #x045E) ; <ISOcyr2> # CYRILLIC SMALL LETTER SHORT U
("Ubreve"   #x016C) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH BREVE
("ubreve"   #x016D) ; <ISOlat2> # LATIN SMALL LETTER U WITH BREVE
("Ucirc"    #x00DB) ; <ISOlat1> # LATIN CAPITAL LETTER U WITH CIRCUMFLEX
("ucirc"    #x00FB) ; <ISOlat1> # LATIN SMALL LETTER U WITH CIRCUMFLEX
("Ucy"      #x0423) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER U
("ucy"      #x0443) ; <ISOcyr1> # CYRILLIC SMALL LETTER U
("Udblac"   #x0170) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
("udblac"   #x0171) ; <ISOlat2> # LATIN SMALL LETTER U WITH DOUBLE ACUTE
("udiagr"   #x03B0) ; <ISOgrk2> # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
("Udigr"    #x03AB) ; <ISOgrk2> # GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
("udigr"    #x03CB) ; <ISOgrk2> # GREEK SMALL LETTER UPSILON WITH DIALYTIKA
("Ugr"      #x03A5) ; <ISOgrk1> # GREEK CAPITAL LETTER UPSILON
("ugr"      #x03C5) ; <ISOgrk1> # GREEK SMALL LETTER UPSILON
("Ugrave"   #x00D9) ; <ISOlat1> # LATIN CAPITAL LETTER U WITH GRAVE
("ugrave"   #x00F9) ; <ISOlat1> # LATIN SMALL LETTER U WITH GRAVE
("uharl"    #x21BF) ; <ISOamsa> # UPWARDS HARPOON WITH BARB LEFTWARDS
("uharr"    #x21BE) ; <ISOamsa> # UPWARDS HARPOON WITH BARB RIGHTWARDS
("uhblk"    #x2580) ; <ISOpub> # UPPER HALF BLOCK
("ulcorn"   #x231C) ; <ISOamsc> # TOP LEFT CORNER
("ulcrop"   #x230F) ; <ISOpub> # TOP LEFT CROP
("Umacr"    #x016A) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH MACRON
("umacr"    #x016B) ; <ISOlat2> # LATIN SMALL LETTER U WITH MACRON
("uml"      #x00A8) ; <ISOdia> # DIAERESIS
("Uogon"    #x0172) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH OGONEK
("uogon"    #x0173) ; <ISOlat2> # LATIN SMALL LETTER U WITH OGONEK
("uplus"    #x228E) ; <ISOamsb> # MULTISET UNION
("Upsi"     #x03A5) ; <ISOgrk3> # GREEK CAPITAL LETTER UPSILON
("upsi"     #x03C5) ; <ISOgrk3> # GREEK SMALL LETTER UPSILON
("upsih"    #x03D2) ; <HTMLsymbol> # GREEK UPSILON WITH HOOK SYMBOL
("Upsilon"  #x03A5) ; <HTMLsymbol> # GREEK CAPITAL LETTER UPSILON
("upsilon"  #x03C5) ; <HTMLsymbol> # GREEK SMALL LETTER UPSILON
("urcorn"   #x231D) ; <ISOamsc> # TOP RIGHT CORNER
("urcrop"   #x230E) ; <ISOpub> # TOP RIGHT CROP
("Uring"    #x016E) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH RING ABOVE
("uring"    #x016F) ; <ISOlat2> # LATIN SMALL LETTER U WITH RING ABOVE
("Utilde"   #x0168) ; <ISOlat2> # LATIN CAPITAL LETTER U WITH TILDE
("utilde"   #x0169) ; <ISOlat2> # LATIN SMALL LETTER U WITH TILDE
("utri"     #x25B5) ; <ISOpub> # WHITE UP-POINTING SMALL TRIANGLE
("utrif"    #x25B4) ; <ISOpub> # BLACK UP-POINTING SMALL TRIANGLE
("Uuml"     #x00DC) ; <ISOlat1> # LATIN CAPITAL LETTER U WITH DIAERESIS
("uuml"     #x00FC) ; <ISOlat1> # LATIN SMALL LETTER U WITH DIAERESIS
("varr"     #x2195) ; <ISOamsa> # UP DOWN ARROW
("vArr"     #x21D5) ; <ISOamsa> # UP DOWN DOUBLE ARROW
("Vcy"      #x0412) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER VE
("vcy"      #x0432) ; <ISOcyr1> # CYRILLIC SMALL LETTER VE
("vdash"    #x22A2) ; <ISOamsr> # RIGHT TACK
("vDash"    #x22A8) ; <ISOamsr> # TRUE
("Vdash"    #x22A9) ; <ISOamsr> # FORCES
("veebar"   #x22BB) ; <ISOamsr> # XOR
("vellip"   #x22EE) ; <ISOpub> # VERTICAL ELLIPSIS
("verbar"   #x007C) ; <ISOnum> # VERTICAL LINE
("Verbar"   #x2016) ; <ISOtech> # DOUBLE VERTICAL LINE
("vltri"    #x22B2) ; <ISOamsr> # NORMAL SUBGROUP OF
("vprime"   #x2032) ; <ISOamso> # PRIME
("vprop"    #x221D) ; <ISOamsr> # PROPORTIONAL TO
("vrtri"    #x22B3) ; <ISOamsr> # CONTAINS AS NORMAL SUBGROUP
("vsubnE"   #x228A) ; <ISOamsn> # SUBSET OF WITH NOT EQUAL TO
("vsubne"   #x228A) ; <ISOamsn> # SUBSET OF WITH NOT EQUAL TO
("vsupne"   #x228B) ; <ISOamsn> # SUPERSET OF WITH NOT EQUAL TO
("vsupnE"   #x228B) ; <ISOamsn> # SUPERSET OF WITH NOT EQUAL TO
("Vvdash"   #x22AA) ; <ISOamsr> # TRIPLE VERTICAL BAR RIGHT TURNSTILE
("Wcirc"    #x0174) ; <ISOlat2> # LATIN CAPITAL LETTER W WITH CIRCUMFLEX
("wcirc"    #x0175) ; <ISOlat2> # LATIN SMALL LETTER W WITH CIRCUMFLEX
("wedgeq"   #x2259) ; <ISOtech> # ESTIMATES
("weierp"   #x2118) ; <ISOamso> # SCRIPT CAPITAL P
("wreath"   #x2240) ; <ISOamsb> # WREATH PRODUCT
("xcirc"    #x25CB) ; <ISOamsb> # WHITE CIRCLE
("xdtri"    #x25BD) ; <ISOamsb> # WHITE DOWN-POINTING TRIANGLE
("Xgr"      #x039E) ; <ISOgrk1> # GREEK CAPITAL LETTER XI
("xgr"      #x03BE) ; <ISOgrk1> # GREEK SMALL LETTER XI
("xhArr"    #x2194) ; <ISOamsa> # LEFT RIGHT ARROW
("xharr"    #x2194) ; <ISOamsa> # LEFT RIGHT ARROW
("Xi"       #x039E) ; <ISOgrk3> # GREEK CAPITAL LETTER XI
("xi"       #x03BE) ; <ISOgrk3> # GREEK SMALL LETTER XI
("xlArr"    #x21D0) ; <ISOamsa> # LEFTWARDS DOUBLE ARROW
("xrArr"    #x21D2) ; <ISOamsa> # RIGHTWARDS DOUBLE ARROW
("xutri"    #x25B3) ; <ISOamsb> # WHITE UP-POINTING TRIANGLE
("Yacute"   #x00DD) ; <ISOlat1> # LATIN CAPITAL LETTER Y WITH ACUTE
("yacute"   #x00FD) ; <ISOlat1> # LATIN SMALL LETTER Y WITH ACUTE
("YAcy"     #x042F) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER YA
("yacy"     #x044F) ; <ISOcyr1> # CYRILLIC SMALL LETTER YA
("Ycirc"    #x0176) ; <ISOlat2> # LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
("ycirc"    #x0177) ; <ISOlat2> # LATIN SMALL LETTER Y WITH CIRCUMFLEX
("Ycy"      #x042B) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER YERU
("ycy"      #x044B) ; <ISOcyr1> # CYRILLIC SMALL LETTER YERU
("yen"      #x00A5) ; <ISOnum> # YEN SIGN
("YIcy"     #x0407) ; <ISOcyr2> # CYRILLIC CAPITAL LETTER YI
("yicy"     #x0457) ; <ISOcyr2> # CYRILLIC SMALL LETTER YI
("YUcy"     #x042E) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER YU
("yucy"     #x044E) ; <ISOcyr1> # CYRILLIC SMALL LETTER YU
("yuml"     #x00FF) ; <ISOlat1> # LATIN SMALL LETTER Y WITH DIAERESIS
("Yuml"     #x0178) ; <ISOlat2> # LATIN CAPITAL LETTER Y WITH DIAERESIS
("Zacute"   #x0179) ; <ISOlat2> # LATIN CAPITAL LETTER Z WITH ACUTE
("zacute"   #x017A) ; <ISOlat2> # LATIN SMALL LETTER Z WITH ACUTE
("Zcaron"   #x017D) ; <ISOlat2> # LATIN CAPITAL LETTER Z WITH CARON
("zcaron"   #x017E) ; <ISOlat2> # LATIN SMALL LETTER Z WITH CARON
("Zcy"      #x0417) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER ZE
("zcy"      #x0437) ; <ISOcyr1> # CYRILLIC SMALL LETTER ZE
("Zdot"     #x017B) ; <ISOlat2> # LATIN CAPITAL LETTER Z WITH DOT ABOVE
("zdot"     #x017C) ; <ISOlat2> # LATIN SMALL LETTER Z WITH DOT ABOVE
("Zeta"     #x0396) ; <HTMLsymbol> # GREEK CAPITAL LETTER ZETA
("zeta"     #x03B6) ; <ISOgrk3> # GREEK SMALL LETTER ZETA
("Zgr"      #x0396) ; <ISOgrk1> # GREEK CAPITAL LETTER ZETA
("zgr"      #x03B6) ; <ISOgrk1> # GREEK SMALL LETTER ZETA
("ZHcy"     #x0416) ; <ISOcyr1> # CYRILLIC CAPITAL LETTER ZHE
("zhcy"     #x0436) ; <ISOcyr1> # CYRILLIC SMALL LETTER ZHE
("zwj"      #x200D) ; <HTMLspecial> # ZERO WIDTH JOINER
("zwnj"     #x200C) ; <HTMLspecial> # ZERO WIDTH NON-JOINER

)
