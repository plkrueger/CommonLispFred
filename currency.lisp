;; currency.lisp


#|
The MIT license.

Copyright (c) 2014 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

#|
This file provides world currency data and access mechanisms.
|#

(defpackage :currency
  (:use :common-lisp)
  (:export
   currencies-within-string
   currency-text
   currency-short-text
   country-currencies
   countries-using-currency
   words))

(in-package :currency)

(defvar *base-parse-table* nil) ;; used to retrieve country code given currency text

(defvar *country-currency-hash* nil) ;; retrieve currency-codes using country name

(defvar *currency-text-hash* nil) ;; retrieve currency text using currency code

(defvar *currency-countries* nil) ;; retrieve countries that use the currency specified by the currency code

;; following derived from data at http://en.wikipedia.org/wiki/List_of_circulating_currencies (as of September 2014)
(defvar *country-currencies*
  '(("Abkhazia" "Abkhazian apsar" :none "Russian ruble" :RUB)
    ("Afghanistan" "Afghan afghani" :AFN)
    ("Akrotiri and Dhekelia" "Euro" :EUR)
    ("Albania" "Albanian lek" :ALL)
    ("Alderney" "Alderney pound" :none "British pound" :GBP "Guernsey pound" :GGP)
    ("Algeria" "Algerian dinar" :DZD)
    ("Andorra" "Euro" :EUR)
    ("Angola Angolan" "kwanza" :AOA)
    ("Anguilla" "East Caribbean dollar" :XCD)
    ("Antigua and Barbuda" "East Caribbean dollar" :XCD)
    ("Argentina" "Argentine peso" :ARS)
    ("Armenia" "Armenian dram" :AMD)
    ("Aruba" "Aruban florin" :AWG)
    ("Ascension Island" "Ascension pound" :none "Saint Helena pound" :SHP)
    ("Australia" "Australian dollar" :AUD)
    ("Austria" "Euro" :EUR)
    ("Azerbaijan" "Azerbaijani manat" :AZN)
    ("Bahamas" "Bahamian dollar" :BSD)
    ("The Bahamas" "Bahamian dollar" :BSD)
    ("Bahrain" "Bahraini dinar" :BHD)
    ("Bangladesh" "Bangladeshi taka" :BDT)
    ("Barbados" "Barbadian dollar" :BBD)
    ("Belarus" "Belarusian ruble" :BYR)
    ("Belgium" "Euro" :EUR)
    ("Belize" "Belize dollar" :BZD)
    ("Benin" "West African CFA franc" :XOF)
    ("Bermuda" "Bermudian dollar" :BMD)
    ("Bhutan" "Bhutanese ngultrum" :BTN "Indian rupee" :INR)
    ("Bolivia" "Bolivian boliviano" :BOB)
    ("Bonaire" "United States dollar" :USD)
    ("Bosnia and Herzegovina" "Bosnia and Herzegovina convertible mark" :BAM)
    ("Botswana" "Botswana pula" :BWP)
    ("Brazil" "Brazilian real" :BRL)
    ("British Indian Ocean Territory" "United States dollar" :USD)
    ("British Virgin Islands" "British Virgin Islands dollar" :none "United States dollar" :USD)
    ("Brunei" "Brunei dollar" :BND "Singapore dollar" :SGD)
    ("Bulgaria" "Bulgarian lev" :BGN)
    ("Burkina Faso" "West African CFA franc" :XOF)
    ("Burma" "Burmese kyat" :MMK)
    ("Burundi" "Burundian franc" :BIF)
    ("Cambodia" "Cambodian riel" :KHR "United States dollar" :USD)
    ("Cameroon" "Central African CFA franc" :XAF)
    ("Canada" "Canadian dollar" :CAD)
    ("Cape Verde" "Cape Verdean escudo" :CVE)
    ("Cayman Islands" "Cayman Islands dollar" :KYD)
    ("Central African Republic" "Central African CFA franc" :XAF)
    ("Chad" "Central African CFA franc" :XAF)
    ("Chile" "Chilean peso" :CLP)
    ("China" "Chinese yuan" :CNY "yuan" :CNY)
    ("People's Republic of China" "Chinese yuan" :CNY)
    ("Cocos (Keeling) Islands" "Australian dollar" :AUD)
    ("Colombia" "Colombian peso" :COP)
    ("Comoros" "Comorian franc" :KMF)
    ("Congo, Democratic Republic of the" "Congolese franc" :CDF)
    ("Democratic Republic of the Congo" "Congolese franc" :CDF)
    ("Congo, Republic of the" "Central African CFA franc" :XAF)
    ("Republic of the Congo" "Central African CFA franc" :XAF)
    ("Cook Islands" "New Zealand dollar" :NZD "Cook Islands dollar" :none)
    ("Costa Rica" "Costa Rican colón" :CRC "Costa Rican colon" :CRC)
    ("Côte d'Ivoire" "West African CFA franc" :XOF)
    ("Croatia" "Croatian kuna" :HRK)
    ("Cuba" "Cuban convertible peso" :CUC "Cuban peso" :CUP)
    ("Curaçao" "Netherlands Antillean guilder" :ANG)
    ("Cyprus" "Euro" :EUR)
    ("Czech Republic" "Czech koruna" :CZK)
    ("Denmark" "Danish krone" :DKK)
    ("Djibouti" "Djiboutian franc" :DJF)
    ("Dominica" "East Caribbean dollar" :XCD)
    ("Dominican Republic" "Dominican peso" :DOP)
    ("East Timor" "United States dollar" :USD)
    ("Ecuador" "United States dollar" :USD)
    ("Egypt" "Egyptian pound" :EGP)
    ("El Salvador" "United States dollar" :USD)
    ("Equatorial Guinea" "Central African CFA franc" :XAF)
    ("Eritrea" "Eritrean nakfa" :ERN)
    ("Estonia" "Euro" :EUR)
    ("Ethiopia" "Ethiopian birr" :ETB)
    ("Falkland Islands" "Falkland Islands pound" :FKP)
    ("Faroe Islands" "Danish krone" :DKK "Faroese króna" :none "Faroese krona" :none)
    ("Fiji" "Fijian dollar" :FJD)
    ("Finland" "Euro" :EUR)
    ("France" "Euro" :EUR)
    ("French Polynesia" "CFP franc" :XPF)
    ("Gabon" "Central African CFA franc" :XAF)
    ("Gambia, The" "Gambian dalasi" :GMD)
    ("The Gambia" "Gambian dalasi" :GMD)
    ("Georgia" "Georgian lari" :GEL)
    ("Germany" "Euro" :EUR)
    ("Ghana" "Ghana cedi" :GHS)
    ("Gibraltar" "Gibraltar pound" :GIP)
    ("Greece" "Euro" :EUR)
    ("Grenada" "East Caribbean dollar" :XCD)
    ("Guatemala" "Guatemalan quetzal" :GTQ)
    ("Guernsey" "British pound" :GBP "Guernsey pound" :GGP)
    ("Guinea" "Guinean franc" :GNF)
    ("Guinea-Bissau" "West African CFA franc" :XOF)
    ("Guyana" "Guyanese dollar" :GYD)
    ("Haiti" "Haitian gourde" :HTG)
    ("Honduras" "Honduran lempira" :HNL)
    ("Hong Kong" "Hong Kong dollar" :HKD)
    ("Hungary" "Hungarian forint" :HUF)
    ("Iceland" "Icelandic króna" :ISK "Icelandic krona" :ISK)
    ("India" "Indian rupee" :INR "rupee" :INR)
    ("Indonesia" "Indonesian rupiah" :IDR)
    ("Iran" "Iranian rial" :IRR)
    ("Iraq" "Iraqi dinar" :IQD)
    ("Ireland" "Euro" :EUR)
    ("Isle of Man" "British pound" :GBP "Manx pound" :IMP)
    ("Israel" "Israeli new shekel" :ILS "shekel" :ILS)
    ("Italy" "Euro" :EUR)
    ("Jamaica" "Jamaican dollar" :JMD)
    ("Japan" "Japanese yen" :JPY)
    ("Jersey" "British pound" :GBP "Jersey pound" :JEP)
    ("Jordan" "Jordanian dinar" :JOD)
    ("Kazakhstan" "Kazakhstani tenge" :KZT)
    ("Kenya" "Kenyan shilling" :KES)
    ("Kiribati" "Australian dollar" :AUD "Kiribati dollar" :none)
    ("Korea, North" "North Korean won" :KPW)
    ("Korea, South" "South Korean won" :KRW "Korean won" :KRW "won" :KRW)
    ("Kosovo" "Euro" :EUR)
    ("Kuwait" "Kuwaiti dinar" :KWD)
    ("Kyrgyzstan" "Kyrgyzstani som" :KGS)
    ("Laos" "Lao kip" :LAK)
    ("Latvia" "Euro" :EUR)
    ("Lebanon" "Lebanese pound" :LBP)
    ("Lesotho" "Lesotho loti" :LSL "South African rand" :ZAR)
    ("Liberia" "Liberian dollar" :LRD)
    ("Libya" "Libyan dinar" :LYD)
    ("Liechtenstein" "Swiss franc" :CHF)
    ("Lithuania" "Lithuanian litas" :LTL)
    ("Luxembourg" "Euro" :EUR)
    ("Macau" "Macanese pataca" :MOP)
    ("Macedonia, Republic of" "Macedonian denar" :MKD)
    ("Madagascar" "Malagasy ariary" :MGA)
    ("Malawi" "Malawian kwacha" :MWK)
    ("Malaysia" "Malaysian ringgit" :MYR)
    ("Maldives" "Maldivian rufiyaa" :MVR)
    ("Mali" "West African CFA franc" :XOF)
    ("Malta" "Euro" :EUR)
    ("Marshall Islands" "United States dollar" :USD)
    ("Mauritania" "Mauritanian ouguiya" :MRO)
    ("Mauritius" "Mauritian rupee" :MUR)
    ("Mexico" "Mexican peso" :MXN)
    ("Micronesia" "Micronesian dollar" :none "United States dollar" :USD)
    ("Moldova" "Moldovan leu" :MDL)
    ("Monaco" "Euro" :EUR)
    ("Mongolia" "Mongolian tögrög" :MNT)
    ("Montenegro" "Euro" :EUR)
    ("Montserrat" "East Caribbean dollar" :XCD)
    ("Morocco" "Moroccan dirham" :MAD)
    ("Mozambique" "Mozambican metical" :MZN)
    ("Nagorno-Karabakh Republic" "Armenian dram" :AMD "Nagorno-Karabakh dram" :none)
    ("Namibia" "Namibian dollar" :NAD "South African rand" :ZAR)
    ("Nauru" "Australian dollar" :AUD "Nauruan dollar" :none)
    ("Nepal" "Nepalese rupee" :NPR)
    ("Netherlands" "Euro" :EUR)
    ("New Caledonia" "CFP franc" :XPF)
    ("New Zealand" "New Zealand dollar" :NZD)
    ("Nicaragua" "Nicaraguan córdoba" :NIO)
    ("Niger" "West African CFA franc" :XOF)
    ("Nigeria" "Nigerian naira" :NGN)
    ("Niue" "New Zealand dollar" :NZD "Niue dollar" :none)
    ("Northern Cyprus" "Turkish lira" :TRY)
    ("Norway" "Norwegian krone" :NOK "Norwegian kroner" :NOK)
    ("Oman" "Omani rial" :OMR)
    ("Pakistan" "Pakistani rupee" :PKR)
    ("Palau" "Palauan dollar" :none "United States dollar" :USD)
    ("Palestine" "Israeli new shekel" :ILS "Jordanian dinar" :JOD)
    ("Panama" "Panamanian balboa" :PAB "United States dollar" :USD)
    ("Papua New Guinea" "Papua New Guinean kina" :PGK)
    ("Paraguay" "Paraguayan guaraní" :PYG)
    ("Peru" "Peruvian nuevo sol" :PEN)
    ("Philippines" "Philippine peso" :PHP)
    ("Pitcairn Islands" "New Zealand dollar" :NZD "Pitcairn Islands dollar" :none)
    ("Poland" "Polish złoty" :PLN "Polish zloty" :PLN)
    ("Portugal" "Euro" :EUR)
    ("Qatar" "Qatari riyal" :QAR)
    ("Romania" "Romanian leu" :RON)
    ("Russia" "Russian ruble" :RUB "ruble" :RUB)
    ("Rwanda" "Rwandan franc" :RWF)
    ("Saba" "United States dollar" :USD)
    ("Sahrawi Republic" "Algerian dinar" :DZD "Mauritanian ouguiya" :MRO "Moroccan dirham" :MAD "Sahrawi peseta" :none)
    ("Saint Helena" "Saint Helena pound" :SHP)
    ("Saint Kitts and Nevis" "East Caribbean dollar" :XCD)
    ("Saint Lucia" "East Caribbean dollar" :XCD)
    ("Saint Vincent and the Grenadines" "East Caribbean dollar" :XCD)
    ("Samoa" "Samoan tālā" :WST "Samoan tala" :WST)
    ("San Marino" "Euro" :EUR)
    ("São Tomé and Príncipe" "São Tomé and Príncipe dobra" :STD "Sao Tome and Príncipe dobra" :STD)
    ("Saudi Arabia" "Saudi riyal" :SAR)
    ("Senegal" "West African CFA franc" :XOF)
    ("Serbia" "Serbian dinar" :RSD)
    ("Seychelles" "Seychellois rupee" :SCR)
    ("Sierra Leone" "Sierra Leonean leone" :SLL)
    ("Singapore" "Brunei dollar" :BND "Singapore dollar" :SGD)
    ("Sint Eustatius" "United States dollar" :USD)
    ("Sint Maarten" "Netherlands Antillean guilder" :ANG)
    ("Slovakia" "Euro" :EUR)
    ("Slovenia" "Euro" :EUR)
    ("Solomon Islands" "Solomon Islands dollar" :SBD)
    ("Somalia" "Somali shilling" :SOS)
    ("Somaliland" "Somaliland shilling" :none)
    ("South Africa" "South African rand" :ZAR)
    ("South Georgia and the South Sandwich Islands" "British pound" :GBP "South Georgia and the South Sandwich Islands pound" :none)
    ("South Ossetia" "Russian ruble" :RUB)
    ("Spain" "Euro" :EUR)
    ("South Sudan" "South Sudanese pound" :SSP)
    ("Sri Lanka" "Sri Lankan rupee" :LKR)
    ("Sudan" "Sudanese pound" :SDG)
    ("Suriname" "Surinamese dollar" :SRD)
    ("Swaziland" "Swazi lilangeni" :SZL)
    ("Sweden" "Swedish krona" :SEK)
    ("Switzerland" "Swiss franc" :CHF)
    ("Syria" "Syrian pound" :SYP)
    ("Taiwan" "New Taiwan dollar" :TWD)
    ("Tajikistan" "Tajikistani somoni" :TJS)
    ("Tanzania" "Tanzanian shilling" :TZS)
    ("Thailand" "Thai baht" :THB)
    ("Togo" "West African CFA franc" :XOF)
    ("Tonga" "Tongan paʻanga" :TOP)
    ("Transnistria" "Transnistrian ruble" :PRB)
    ("Trinidad and Tobago" "Trinidad and Tobago dollar" :TTD)
    ("Tristan da Cunha" "Saint Helena pound" :SHP "Tristan da Cunha pound" :none)
    ("Tunisia" "Tunisian dinar" :TND)
    ("Turkey" "Turkish lira" :TRY)
    ("Turkmenistan" "Turkmenistan manat" :TMT)
    ("Turks and Caicos Islands" "United States dollar" :USD)
    ("Tuvalu" "Australian dollar" :AUD "Tuvaluan dollar" :none)
    ("Uganda" "Ugandan shilling" :UGX)
    ("Ukraine" "Ukrainian hryvnia" :UAH)
    ("United Arab Emirates" "United Arab Emirates dirham" :AED)
    ("United Kingdom" "British pound" :GBP "pound" :GBP)
    ("United States" "United States dollar" :USD "U.S. dollar" :USD "dollar" :USD)
    ("Uruguay" "Uruguayan peso" :UYU)
    ("Uzbekistan" "Uzbekistani som" :UZS)
    ("Vanuatu" "Vanuatu vatu" :VUV)
    ("Vatican City" "Euro" :EUR)
    ("Venezuela" "Venezuelan bolívar" :VEF)
    ("Vietnam" "Vietnamese đồng" :VND "Vietnamese đong" :VND)
    ("Wallis and Futuna" "CFP franc" :XPF)
    ("Yemen" "Yemeni rial" :YER)
    ("Zambia" "Zambian kwacha" :ZMW)
    ("Zimbabwe" "Botswana pula" :BWP "British pound" :GBP "Euro" :EUR "South African rand" :ZAR "United States dollar" :USD)
    ("Any" "National Currency Unit" :NCU)))

(defun populate-hash-tables ()
  (setf *country-currency-hash* (make-hash-table :test #'equalp))
  (setf *base-parse-table* (make-hash-table :test #'equalp))
  (setf *currency-text-hash* (make-hash-table :test #'eq))
  (setf *currency-countries* (make-hash-table :test #'eq))
  (dolist (ctry-info *country-currencies* nil)
    (let ((ctry (first ctry-info))
          (curr-codes nil))
      (do ((curr-list (rest ctry-info)
                      (cddr curr-list)))
          ((null curr-list))
        (let ((ctext (first curr-list))
              (ccode (second curr-list)))
          (add-to-text-parse-tree (words ctext) ccode)
          (pushnew ctext (gethash ccode *currency-text-hash* nil) :test #'string-equal) ;; some currencies have multiple names (e.g. Krone Kroner)
          (pushnew ctry (gethash ccode *currency-countries* nil))
          (pushnew ccode curr-codes)))
      (setf (gethash ctry *country-currency-hash*) curr-codes))))

(defun add-to-text-parse-tree (ctext-words ccode &optional (parse-hash *base-parse-table*))
  ;; Make it possible to retrieve the currency code when given the ctext
  ;; Each parse-tree node has a value that is either a country code if it is valid to stop the parse at
  ;; that point and get a valid code or another parse-tree node if the parse can continue to the next
  ;; level.
  (let* ((first-word (first ctext-words))
         (second-word (second ctext-words))
         (existing-entry (gethash first-word parse-hash)))
      (cond (second-word
             (unless existing-entry
               (setf existing-entry (make-hash-table :test #'equalp)))
             (add-to-text-parse-tree (rest ctext-words) ccode existing-entry))
            (first-word
             (if existing-entry
                 (unless (eq existing-entry ccode)
                   (error "Same path leads to country codes ~s and ~s"
                          (symbol-name existing-entry)
                          (symbol-name ccode)))
                 (setf existing-entry ccode)))
            (t
             (error "add-to-text-parse-tree given no words")))
    (setf (gethash first-word parse-hash) existing-entry)
    (when (symbolp existing-entry)
      ;; make sure we can get to plural forms of the currency text as well
      (setf (gethash (concatenate 'string first-word "s") parse-hash) existing-entry))))

;; functions to return currency info

(defun currencies-within-string (str)
  ;; Returns a list of currency codes corresponding to valid currency descriptions within str.
  (do ((wrds (words str))
       (found-currency-codes nil))
      ((null wrds) (nreverse found-currency-codes))
    (multiple-value-bind (curr-found wrds-used)
                         (match-words-to-currency wrds)
      (if curr-found
          (progn
            (push curr-found found-currency-codes)
            (setf wrds (subseq wrds wrds-used)))
          (setf wrds (rest wrds))))))

(defun match-words-to-currency (wrd-list &optional (parse-hash *base-parse-table*))
  ;; return two values:
  ;;   the currency code corresponding to the first N words of wrd-list if one can be found
  ;;   the number of words used to traverse the parse tree to a valid currency (i.e. N).
  (let ((entry (gethash (first wrd-list) parse-hash)))
    (typecase entry
      (hash-table
       (multiple-value-bind (curr-found wrds-used)
                            (match-words-to-currency (rest wrd-list) entry)
         (if curr-found
             (values curr-found (1+ wrds-used)))))
      (keyword
       (values entry 1))
      (t
       nil))))

(defun currency-text (ccode)
  (reduce #'max-length (gethash ccode *currency-text-hash*)))

(defun currency-short-text (ccode)
  (reduce #'min-length (gethash ccode *currency-text-hash*)))

(defun country-currencies (ctry-str)
  (gethash ctry-str *country-currency-hash* nil))

(defun countries-using-currency (ccode)
  (gethash ccode *currency-countries* nil))

;; Utility functions

(defun max-length (str1 str2)
  (if (>= (length str1) (length str2))
      str1
      str2))

(defun min-length (str1 str2)
  (if (<= (length str1) (length str2))
      str1
      str2))

(defun unescaped-delim-pos (str delim-string &optional (start 0))
  ;; Find next position of a delimiter unless previous character was a "\"
  (do* ((last-pos (1- (length str)))
        (pos start
             (1+ pos))
        (last-escape-p nil
                       (char= ch #\\))
        (ch (elt str pos)
            (elt str pos))
        (delim-p (and (find ch delim-string :test #'char=) (not last-escape-p))
                 (and (find ch delim-string :test #'char=) (not last-escape-p))))
       ((or delim-p (>= pos last-pos)) (when delim-p pos))))

(defun words (str &optional (delim-string " ") (return-delims nil))
  ;; given a string, return a list of space-delimited words in it
  ;; If return-delims is t then non-space delimiters are returned as single
  ;; character words.
  (when (string str)
    (do* ((words nil)
          (s (string str))
          (strt 0
                (and p (< p (1- (length s))) (1+ p)))
          (p (unescaped-delim-pos s delim-string)
             (and strt (unescaped-delim-pos s delim-string strt)))
          (delim-found (and p (elt s p))
                       (and p (elt s p)))
          (wrd (remove #\\
                       (when strt
                         (if p
                             (subseq s strt p)
                             (subseq s strt)))
                       :test #'char=)
               (remove #\\
                       (when strt
                         (if p
                             (subseq s strt p)
                             (subseq s strt)))
                       :test #'char=)))
         ((null wrd) (nreverse words))
      (unless (string= wrd "")
        (push wrd words))
      (when (and return-delims delim-found (not (char= delim-found #\space)))
        (push (string (elt s p)) words)))))

(populate-hash-tables)

(provide :currency)
