;; hist-date.lisp

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
Unfortunately, standard Common Lisp dates can only represent dates after Jan 1, 1900. Since many
historical data series have values that precede this date, we need to use an alternative representation.
It would always be possible to simple use string values, but for computational purposes (i.e. comparing
dates and computing differences between dates) a better representation is desirable.

This file implements such a package. Basically dates are represented as an integer that encodes a
date as a day-number (with day 0 being March 1, 0000) left shifted by 17 bits (i.e. multiplying by 131072)
plus the number of seconds in the time of day. This makes dates numerically comparable (i.e. later dates 
are > than earlier dates). 

Methods are provided to construct and print such dates and to do various sorts
of useful computation on them. Time zones are ignored so it is not possible to compare times around the
world. Daylight saving time is also ignored.

This uses algorithms for the Gregorian calendar, so no dates prior to October 1, 1582 can be representd
accurately. You can physically encode dates as early as March 1, 0000, but the day of week computation for
dates earlier than October 1, 1582 will be incorrect, as will date differences. No checks are done to see 
that dates are within the range of the Gregorian calendar.

Computations assume that every year that is exactly divisible by four is a leap year, except for years
that are exactly divisible by 100, but these centurial years are leap years if they are exactly divisible
by 400. For example, the years 1700, 1800, and 1900 are not leap years, but the year 2000 is.

Functions are also provided to convert between this data format and standard common lisp dates whenever
that is possible (i.e. for dates after Jan 1, 1900).

|#

(defpackage :hist-date
  (:use :common-lisp)
  (:export
   ;;; Encoding hist-dates
   hist-date
   hist-day
   intl-string-to-date

   ;;; Decoding hist-dates
   abbrev-day-of-wk
   date-list
   day-of-wk
   dt-yr
   hist-date-day
   hist-date-hr-min-sec
   hist-date-time-secs
   hist-date-yr-month-day
   hist-day-yr-month-day
   mmddyy-list

   ;;; Converting hist-dates to/from common lisp dates
   hist-date-to-lisp-date
   lisp-date-to-hist-date

   ;;; Constructing commonly used hist-dates
   last-dt-of-month
   next-day
   next-month
   next-year
   now
   prev-day
   prev-month
   prev-year
   todays-date

   ;;; Computing with hist-dates
   date-diff
   days-diff
   days-from
   inc-date
   inc-days
   inc-months
   inc-years
   months-diff
   same-day-p
   years-diff
   +days

   ;;; Iterators for hist-dates
   do-dates
   do-interval-dates
   do-months

   ;;; Formatting hist-dates as strings
   date-string
   day-of-wk-abbrev-string
   day-of-wk-string
   intl-date-string
   mmdd-string
   mmyy-string
   short-date-string
   short-time-string
   time-string
   yr-string))

(in-package :hist-date)

;;; Encoding hist-dates

(defun hist-date (year month day &optional (hr 0) (min 0) (sec 0))
  ;; encode date and optionally time values into a hist-date integer
  ;; any day greater than is allowed for the month specified is transformed into
  ;; the greatest day allowed.
  (let* ((max-dd (if (and (eql month 2)
                          (not (logtest year 3))
                          (or (eql 0 (mod year 400))
                              (not (eql 0 (mod year 100)))))
                     29
                     (svref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))
         (day (hist-day year month (min max-dd day))))
    (+ (* day 131072)
       (* hr 3600)
       (* min 60)
       sec)))

(defun hist-day (year month day)
  ;; create a day index for the arguments
  (let* ((adj-mm (mod (+ month 9) 12))
         (adj-yr (- year (floor adj-mm 10))))
    (+ (* 365 adj-yr)
       (floor adj-yr 4)
       (- (floor adj-yr 100))
       (floor adj-yr 400)
       (floor (+ (* adj-mm 306) 5) 10)
       (1- day))))

(defun intl-string-to-date (date-str)
  (when (stringp date-str)
    (if (> (length date-str) 18)
        (hist-date (read-from-string (subseq date-str 0 4))
                   (read-from-string (subseq date-str 5 7))
                   (read-from-string (subseq date-str 8 10))
                   (read-from-string (subseq date-str 11 13))
                   (read-from-string (subseq date-str 14 16))
                   (read-from-string (subseq date-str 17 19)))
        (hist-date (read-from-string (subseq date-str 0 4))
                   (read-from-string (subseq date-str 5 7))
                   (read-from-string (subseq date-str 8 10))))))

;;; Decoding hist-dates

(defun hist-day-yr-month-day (hday)
  ;; decode a day into the yr month day components
  (let* ((yr (floor (+ (* 10000 hday) 14780) 3652425))
         (day-of-year (- hday (+ (* yr 365) (floor yr 4) (- (floor yr 100)) (floor yr 400)))))
    (when (minusp day-of-year)
      (setf yr (1- yr))
      (setf day-of-year (- hday (+ (* yr 365) (floor yr 4) (- (floor yr 100)) (floor yr 400)))))
    (let ((month-indx (floor (+ 52 (* 100 day-of-year)) 3060)))
      (values (+ yr (floor (+ month-indx 2) 12))
              (1+ (mod (+ month-indx 2) 12))
              (1+ (- day-of-year (floor (+ (* month-indx 306) 5) 10)))))))

(defun hist-date-day (hdate)
  (floor hdate 131072))

(defun hist-date-yr-month-day (hdate)
  (hist-day-yr-month-day (hist-date-day hdate)))

(defun hist-date-time-secs (hdate)
  (mod hdate 131072))

(defun hist-date-hr-min-sec (hdate)
  (let ((secs (hist-date-time-secs hdate)))
    (multiple-value-bind (hrs secs-left) (floor secs 3600)
      (multiple-value-bind (min sec) (floor secs-left 60)
        (values hrs min sec)))))

(defun day-of-wk (dt)
  (nth (mod (hist-date-day dt) 7) 
       '(:wednesday :thursday :friday :saturday :sunday :monday :tuesday )))

(defun abbrev-day-of-wk (dt)
  (nth (mod (hist-date-day dt) 7) 
       '(:wed :thu :fri :sat :sun :mon :tue)))

(defun dt-yr (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (declare (ignore mm dd))
      yr))

(defun date-list (strt end)
  (do* ((res (list strt) (cons next res))
        (next (next-day strt) (next-day next)))
       ((> next end) (nreverse res))))

(defun mmddyy-list (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (list mm dd yr)))

;;; Converting hist-dates to/from common lisp dates

(defun hist-date-to-lisp-date (dt)
  (multiple-value-bind (yr mm dd) (hist-date-yr-month-day dt)
    (multiple-value-bind (hr min sec) (hist-date-hr-min-sec dt)
      (if (>= yr 1900)
          (encode-universal-time sec min hr dd mm yr)
          (error "Historical date ~a cannot be converted to a lisp date"
                 (date-string dt))))))

(defun lisp-date-to-hist-date (ldate)
  (multiple-value-bind (sec min hr dd mm yr day dst zone)
                       (decode-universal-time ldate)
    (declare (ignore day dst zone))
    (hist-date yr mm dd hr min sec)))

;;; Constructing commonly used hist-dates (some relative to other dates)

(defun now ()
  (multiple-value-bind (sec min hr dd mm yr day dst zone)
                       (decode-universal-time (get-universal-time))
    (declare (ignore day dst zone))
    (hist-date yr mm dd hr min sec)))

(defun todays-date ()
  ;; ignores current time
  (multiple-value-bind (sec min hr dd mm yr day dst zone)
                       (decode-universal-time (get-universal-time))
    (declare (ignore sec min hr day dst zone))
    (hist-date yr mm dd)))

(defun next-day (dt)
  ;; same time on next day
  (+ 131072 dt))

(defun prev-day (dt)
  ;; same time on previous day
  (- dt 131072))

(defun next-month (dt)
  (inc-months dt 1))
                           
(defun prev-month (dt)
  (inc-months dt -1))

(defun next-year (dt)
  (inc-years dt 1))
  
(defun prev-year (dt)
  (inc-years dt -1))

(defun last-dt-of-month (mm yy)
  (hist-date yy mm 31))

;;; Computing with hist-dates

(defun inc-days (dt num-dd)
  (+ (* 131072 num-dd) dt))

(defun inc-months (dt num-mm)
  ;; Note that hist-date will always return a legal date by reducing
  ;; the dd arg to the max allowable for the month.
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (multiple-value-bind (yy-inc new-mm)
                         (floor (+ num-mm mm -1) 12)
      (+ (hist-date-time-secs dt)
         (hist-date (+ yr yy-inc) (1+ new-mm) dd)))))

(defun inc-years (dt num-yy)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (+ (hist-date-time-secs dt)
       (hist-date (+ yr num-yy) mm dd))))

(defun inc-date (dt num interval-type)
  ;; interval-type should be one of :day :month :quarter :year
  (case interval-type
    (:day (inc-days dt num))
    (:week (inc-days dt (* 7 num)))
    (:month (inc-months dt num))
    (:quarter (inc-months dt (* 3 num)))
    (:year (inc-years dt num))))

(defun date-diff (dt1 dt2 &optional (interval-type :day))
  ;; Returns a fractional representation of the difference between the two dates
  ;; ignoring the time of day. If you want to ignore fractions you should call
  ;; truncate on the result which works as desired for negative values as well.
  (case interval-type
    (:day (days-diff dt1 dt2))
    (:week (/ (days-diff dt1 dt2) 7))
    (:month (months-diff dt1 dt2))
    (:quarter (/ (months-diff dt1 dt2) 3))
    (:year (years-diff dt1 dt2))))

(defun days-diff (dt1 dt2)
  (- (hist-date-day dt1) (hist-date-day dt2)))

(defun months-diff (dt1 dt2)
  ;; computes a floating point value of months where the fraction of a month is
  ;; computed using 7 days as .25 of 1 month
  ;; round the result if you want an integer
  ;; This has some use if you want to treat each month as a constant span on a graph
  ;; or something like that. For example (months-between (hist-date 2012 2 3) (hist-date 2012 4 3))
  ;; will return 2.0.
  (multiple-value-bind (yr1 mm1 dd1)
                       (hist-date-yr-month-day dt1)
    (multiple-value-bind (yr2 mm2 dd2)
                       (hist-date-yr-month-day dt2)
      (float (+ (* 12 (- yr1 yr2)) (- mm1 mm2) (/ (- dd1 dd2) 28))))))

(defun years-diff (dt1 dt2)
  ;; computes a floating point value of years where the fraction of a year is
  ;; computed treating each month as 1/12 of a year and each day as 1/365 of 1 year
  ;; round the result if you want an integer
  (multiple-value-bind (yr1 mm1 dd1)
                       (hist-date-yr-month-day dt1)
    (multiple-value-bind (yr2 mm2 dd2)
                       (hist-date-yr-month-day dt2)
      (float (+ (- yr1 yr2) (/ (- mm1 mm2) 12) (/ (- dd1 dd2) 365))))))

(defmethod days-from ((dt1 integer) (dt2 integer))
  (days-diff dt2 dt1))

(defmethod days-from ((dt integer) (day keyword))
  (let* ((abbrev-days '(:sun :mon :tue :wed :thu :fri :sat))
         (days '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))
         (dow (abbrev-day-of-wk dt))
         (today (position dow abbrev-days))
         (day-pos (or (position day days)
                      (position day abbrev-days))))
    (if (<= today day-pos)
      (- day-pos today)
      (- 7 (- today day-pos)))))

(defmethod days-from ((day keyword) (dt integer))
  (let* ((abbrev-days '(:sun :mon :tue :wed :thu :fri :sat))
         (days '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))
         (dow (abbrev-day-of-wk dt))
         (day-pos (position dow abbrev-days))
         (today (or (position day days)
                    (position day abbrev-days))))
    (if (<= today day-pos)
      (- day-pos today)
      (- 7 (- today day-pos)))))

(defmethod days-from ((day1 keyword) (day2 keyword))
  (let* ((abbrev-days '(:sun :mon :tue :wed :thu :fri :sat))
         (days '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))
         (today (or (position day1 days)
                    (position day1 abbrev-days)))
         (day-pos (or (position day2 days)
                      (position day2 abbrev-days))))
    (if (<= today day-pos)
      (- day-pos today)
      (- 7 (- today day-pos)))))

(defun +days (dt days)
  (inc-days dt days))

(defun same-day-p (dt1 dt2)
  (eql (hist-date-day dt1) (hist-date-day dt2)))

;;; Iterators for hist-dates

(defmacro do-dates ((dt start end &optional (return-form nil ret-p)) &rest forms)
  `(do* ((,dt ,start (+ ,dt 131072)))
        ((> ,dt ,end) (if ,ret-p ,return-form (values)))
     ,@forms))

(defmacro do-interval-dates ((dt start end interval &optional (return-form nil ret-p)) &rest forms)
  ;; interval is some number of days
  `(do* ((,dt ,start (+ ,dt (* ,interval 131072))))
        ((> ,dt ,end) (if ,ret-p ,return-form (values)))
     ,@forms))

(defmacro do-months ((dt start end mm-interval &optional (return-form nil ret-p)) &rest forms)
  `(do* ((,dt ,start (inc-months ,dt ,mm-interval)))
        ((> ,dt ,end) (if ,ret-p ,return-form (values)))
     ,@forms))

;;; Formatting hist-dates as strings

(defun day-of-wk-string (dt)
  (nth (mod (hist-date-day dt) 7) 
       '("Wednesday" "Thursday" "Friday" "Saturday" "Sunday" "Monday" "Tuesday")))

(defun day-of-wk-abbrev-string (dt)
  (nth (mod (hist-date-day dt) 7) 
       '("Wed" "Thu" "Fri" "Sat" "Sun" "Mon" "Tue")))

(defun date-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (format nil 
            "~a ~2,'0d/~2,'0d/~2,'0d" 
            (day-of-wk-string dt)
            mm dd (mod yr 100))))

(defun intl-date-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (format nil 
            "~4,'0d-~2,'0d-~2,'0d" yr mm dd)))

(defun short-date-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (format nil 
            "~2,'0d/~2,'0d/~2,'0d" mm dd (mod yr 100))))

(defun short-time-string (dt)
  (multiple-value-bind (hr min sec)
                       (hist-date-hr-min-sec dt)
    (declare (ignore sec))
    (format nil 
            "~2,'0d:~2,'0d" hr min)))

(defun time-string (dt)
  (multiple-value-bind (hr min sec)
                       (hist-date-hr-min-sec dt)
    (format nil 
            "~2,'0d:~2,'0d:~2,'0d on ~a" hr min sec (date-string dt))))

(defun mmdd-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (declare (ignore yr))
    (format nil 
            "~2,'0d/~2,'0d" mm dd)))

(defun mmyy-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (declare (ignore dd))
    (format nil 
            "~2,'0d/~2,'0d" mm (mod yr 100))))

(defun yr-string (dt)
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (declare (ignore mm dd))
    (format nil 
            "~4,'0d" yr)))

(provide :hist-date)
