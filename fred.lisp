;; fred.lisp

#|
The MIT license.

Copyright (c) 2014 Paul L. Krueger

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

By using this software, you are agreeing to be bound by the FRED® API Terms of Use as described by:
       http://api.stlouisfed.org/terms_of_use.html 

This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis.
Information obtained using this API is subject to the "Legal Notices, Information and Disclaimers" described
at https://research.stlouisfed.org/legal.html.

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT 
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

#|

This defines the package used for accessing data from the FRED® facility provided by the St. Louis
Federal Reserve Bank. It provides a Lisp interface to the FRED® API. Users of this interface must
first obtain a user account with the Federal Reserve and then request their own API key for use in
making queries.

The idea behind this implementation is that developers might want to use data obtained both from the FRED®
facility and possibly other sources. So a series of base classes are defined and then FRED-specific
classes are derived from them. Methods provided here are defined either on a base class or
the FRED®-specific class, depending on whether or not they rely on the FRED®-specific API. Users wishing
to implement or use data from other sources may define their own classes that derive from the
base classes and define methods that are specific to those other sources. See below for methods that
must be defined for new classes.

This implemenation loads data from the FRED® facility in a lazy manner, only when a user first attempts to access it.

The FRED® API used here is as documented on the FRED website on 8/14/2014. Subsequent changes to that 
definition are not reflected here. Additions to returned XML forms or reordering of their contents will not prevent
correct functioning of this code although obviously some newly available information may not be retrieved.

|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :fred-classes))

(in-package :fred)

;; Global variables

;; Developers must acqire a FRED® access key and provide it in the call to initialize-fred
;; or initialize the *fred-api-key* var below.
;; To do this you must first have a FRED® account (see http://api.stlouisfed.org/useraccount/regiser/step1)
;; and then request an API key (see http://api.stlouisfed.org/api_key.html).
;; The main FRED® access page is http://api.stlouisfed.org/datatools.html and from there you can access the
;; developer tools page for more information about this API.

(defvar *fred-api-key* nil)

(defvar *all-categories* (make-hash-table :test #'equalp))

(defvar *all-series* (make-hash-table :test #'equalp))

(defvar *all-releases* (make-hash-table :test #'equalp))

(defvar *all-sources* (make-hash-table :test #'equalp))

(defvar *all-tag-groups* (make-hash-table :test #'equalp))

(defvar *all-tags* (make-hash-table :test #'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun conjoin-search-strings (str-list)
  (format nil "~{~a~^+~}" (mapcar #'(lambda (txt)
                                     (substitute-in-string txt " " "%20"))
                                 str-list)))

(defun conjoin-strings-for-query (str-list)
  (format nil "~{~a~^;~}" str-list))

(defun conjoin-dates-for-query (dt-list)
  (format nil "~{~a~^,~}" (mapcar #'fred-date-string dt-list)))

(defun data-index (dt start-dt freq-key)
  ;; compute an index into an array where index 0 corresponds to the start-dt
  ;; specified and freq-key indicates the date increment for each index.
  ;; It is assumed that the dt parameter is of the appropriate form and within
  ;; the date range represented by the array.
  (case freq-key
    (:annual
     (truncate (-dates dt start-dt :year)))
    (:semiannual
     (truncate (/ (-dates dt start-dt :year) 2.0)))
    (:quarterly
     (truncate (-dates dt start-dt :quarter)))
    (:monthly
     (truncate (-dates dt start-dt :month)))
    (:weekly
     (truncate (-dates dt start-dt :week)))
    (:bi-weekly
     (truncate (/ (-dates dt start-dt :week) 2.0)))
    (:daily
     (-dates dt start-dt :day))))

(defun period-dates (start-dt dt freq-key)
  ;; all period dates encoded as 00:00 AM on the first date of the period
  ;; Find the date in the series that immediately preceded the specified date, which
  ;; represents the current period relative to the dt argument.
  ;; Return dates for the prior period, current period, and next period.
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (declare (ignore dd))
    (multiple-value-bind (strt-yr strt-mm strt-dd)
                         (hist-date-yr-month-day start-dt)
      (declare (ignore strt-yr strt-dd))
      (case freq-key
        (:annual
         ;; we do NOT make the assumption that the year starts on 1/1
         ;; we use the series start date to tell us what month to use
         (let ((current-dt (if (>= mm strt-mm)
                               (hist-date yr strt-mm 01)
                               (hist-date (1- yr) strt-mm 01))))
           (values (years- current-dt)
                   current-dt
                   (years+ current-dt))))
        (:semiannual
         ;; we do NOT make the assumption that the half-year periods start on 1/1 and 7/1
         ;; we use the series start date to tell us what months to use
         (let* ((low-mm (if (< strt-mm 7) strt-mm (- strt-mm 6)))
                (hi-mm (if (< strt-mm 7) (+ strt-mm 6) strt-mm))
                (current-dt (cond ((< mm low-mm)
                                   (hist-date (1- yr) hi-mm 01))
                                  ((< mm hi-mm)
                                   (hist-date yr low-mm 01))
                                  (t
                                   (hist-date yr hi-mm 01)))))
           (values (months- current-dt 6)
                   current-dt
                   (months+ current-dt 6))))
        (:quarterly
         ;; we DO make the assumption that quarters start on 1/1, 4/1, 7/1, and 10/1
         (let* ((quarter-indx (floor (1- mm) 3))
                (current-dt (hist-date yr (1+ (* 3 quarter-indx)) 01)))
           (values (date- current-dt 1 :quarter)
                   current-dt
                   (date+ current-dt 1 :quarter))))
        (:monthly
         ;; we DO make the assumption that months start on the first of the month
         (let ((current-dt (hist-date yr mm 01)))
           (values (months- current-dt)
                   current-dt
                   (months+ current-dt))))
        (:weekly
         ;; we do NOT make any assumption about the day of the week on which the series starts
         ;; we use the day of the week of the series start date to tell us.
         (let ((current-dt (weeks+ start-dt (truncate (-dates dt start-dt :week)))))
           (values (date- current-dt 1 :week)
                   current-dt
                   (date+ current-dt 1 :week))))
        (:bi-weekly
         ;; we do NOT make any assumption about the day of the week or week of the year on which the series starts
         ;; we use the day of the week of the series start date to tell us.
         (let ((current-dt (weeks+ start-dt (* 2 (truncate (-dates dt start-dt :week) 2)))))
           (values (date- current-dt 2 :week)
                   current-dt
                   (date+ current-dt 2 :week))))))))

(defun remove-lf-strs (xml-form)
  (let* ((lf-str (make-string 1 :initial-element #\lf))
         (new-form (if (listp xml-form)
                     (remove lf-str xml-form :test #'equal)
                     xml-form)))
    (if (listp new-form)
      (mapcan #'(lambda (form)
                  (list (remove-lf-strs form)))
              new-form)
      new-form)))

(defun fred-string-to-key (fred-str)
  (intern (string-upcase fred-str) (find-package :keyword)))

(defun fred-string-to-freq-key (fred-str)
  (let* ((wrds (mapcar #'string-upcase (words fred-str ",. ")))
         (freq-wrds (intersection wrds 
                                  (list "ANNUAL" "SEMIANNUAL" "QUARTERLY" "MONTHLY" "WEEKLY" "BI-WEEKLY" "DAILY")
                                  :test #'string=)))
    (if freq-wrds
        (intern (first freq-wrds) (find-package :keyword))
        :ANNUAL)))

(defun fred-key-to-string (fred-key &optional (case :capitalize))
  (case case
    (:capitalize
     (string-capitalize (symbol-name fred-key)))
    (:up
     (string-upcase (symbol-name fred-key)))
    (:down
     (string-downcase (symbol-name fred-key)))))

(defun period-indices (start-dt freq obs-dt)
  (multiple-value-bind (prior-period-dt current-period-dt next-period-dt)
                       (period-dates start-dt obs-dt freq)
    (let ((current-indx (data-index current-period-dt start-dt freq)))
      (values prior-period-dt
              (max (1- current-indx) 0)
              current-period-dt
              current-indx
              next-period-dt))))

(defun substitute-in-string (str old-str new-str)
  ;; substitue new-str for old-str wherever it occurs within str and return the result
  (do* ((search-length (length old-str))
        (new-sub-strs nil)
        (first-char-to-match (elt old-str 0))
        (first-pos 0)
        (search-start 0
                      (max first-pos (1+ match-start)))
        (match-start (position first-char-to-match str :test #'char= :start search-start)
                     (position first-char-to-match str :test #'char= :start search-start))
        (match-p (and match-start (string= (subseq str match-start (+ match-start search-length)) old-str))
                 (and match-start (string= (subseq str match-start (+ match-start search-length)) old-str))))
       ((not match-start) (apply #'concatenate 'string (nreverse (cons (subseq str first-pos) new-sub-strs))))
    (when match-p
      (push (subseq str first-pos match-start) new-sub-strs)
      (push (copy-seq new-str) new-sub-strs)
      (setf first-pos (+ match-start search-length)))))

(defmacro nil-if-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (c)
            (declare (ignore c))
            nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRED initialization 

;; either call this function with your api-key or just modify the defvar for *fred-api-key*
;; or call this function with a file in your home directory that contains a quoted string
;; containing your api_key
(defun initialize-fred (&optional (api-key nil))
  (setf *fred-api-key* (or api-key
                           (and (probe-file "~/API_Key.txt")
                                (with-open-file (f "~/API_Key.txt")
                                  (string-downcase (string (read f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object location and removal methods

(defun find-category (id)
  (gethash id *all-categories*))

(defun remove-category (id)
  (remhash id *all-categories*))

(defun find-series (id &optional (transform :lin))
  (gethash (cons id transform) *all-series*))

(defun remove-series (id &optional (transform :lin))
  (remhash (cons id transform) *all-series*))

(defun find-release (id)
  (gethash id *all-releases*))

(defun remove-release (id)
  (remhash id *all-releases*))

(defun find-source (id)
  (gethash id *all-sources*))

(defun remove-source (id)
  (remhash id *all-sources*))

(defun find-tag (name)
  (gethash name *all-tags*))

(defun remove-tag (name)
  (remhash name *all-tags*))

(defun find-tag-group (id)
  (gethash id *all-tag-groups*))

(defun remove-tag-group (id)
  (remhash id *all-tag-groups*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class methods for non-fred-specific classes

;; category

(defmethod initialize-instance :after ((self data-category)
                                       &key
                                       &allow-other-keys)
  (setf (gethash (cat-id self) *all-categories*) self))
    
;; series 

(defmethod initialize-instance :after ((self data-series)
                                       &key
                                       &allow-other-keys)
  (setf (gethash (cons (series-id self) (series-transform self)) *all-series*) self))

(defmethod set-series-observations ((self data-series) obs-list)
  (let* ((num-obs (list-length obs-list))
         (array-sz (1+ (data-index (series-end-dt self) (series-start-dt self) (series-frequency self))))
         (content-obs (if (>= num-obs array-sz)
                          obs-list
                          (append obs-list (make-list (- array-sz num-obs) :initial-element 0)))))
    (setf (slot-value self 'series-observations)
          (make-array (list array-sz)
                      :adjustable t
                      :fill-pointer t
                      :initial-contents content-obs))
    (setf (series-max self) (reduce #'max content-obs))
    (setf (series-min self) (reduce #'min content-obs))
    (setf (series-sum self) (apply #'+ content-obs))
    (setf (series-avg self) (/ (series-sum self) array-sz))
    (slot-value self 'series-observations)))

(defmethod (setf series-observation) (val (self data-series) obs-dt)
  ;; set a single value for a series
  ;; obs-dt must be between series-start-dt and series-end-dt
  (with-slots (series-start-dt series-end-dt series-frequency series-max series-min
                               series-id series-sum series-avg series-observations) self
    (when (or (< obs-dt series-start-dt)
              (> obs-dt series-end-dt))
      (error "~a is not within the range of observed dates for series ~s"
             (fred-date-string obs-dt)
             series-id))
    (multiple-value-bind (prior-period-dt current-period-dt next-period-dt)
                         (period-dates series-start-dt obs-dt series-frequency)
      (declare (ignore prior-period-dt next-period-dt))
      (let* ((indx (data-index current-period-dt series-start-dt series-frequency))
             (old-val (aref series-observations indx)))
        (setf (aref series-observations indx) val)
        (if (> val series-max)
            (setf series-max val)
            (if (= old-val series-max)
                (setf series-max (reduce #'max series-observations))))
        (if (< val series-min)
            (setf series-min val)
            (if (= old-val series-min)
                (setf series-min (reduce #'min series-observations))))
        (decf series-sum old-val)
        (incf series-sum val)
        (setf series-avg (/ series-sum (first (array-dimensions series-observations))))))
    val))

(defmethod slot-unbound ((cl (eql (find-class 'data-series)))
                         (self data-series)
                         (slot (eql 'series-observations)))
  (set-series-observations self nil))

(defmethod series-observation-iterator ((self data-series) &optional (st-dt 0))
  ;; returns an iterator function which returns the next observation date and value each time it is called
  ;; or nil for each after the end date of the series is reached
  (let ((next-date (max st-dt (series-start-dt self)))
        (next-date-interval (case (series-frequency self)
                              (:annual (list 1 :year))
                              (:semiannual (list 6 :month))
                              (:quarterly (list 1 :quarter))
                              (:monthly (list 1 :month))
                              (:weekly (list 1 :week))
                              (:bi-weekly (list 2 :week)))))
    #'(lambda ()
        (multiple-value-prog1
          (if (> next-date (series-end-dt self))
              (values nil nil)
              (values next-date (series-observation self next-date)))
          (setf next-date (date+ next-date (first next-date-interval) (second next-date-interval)))))))

;; release

(defmethod initialize-instance :after ((self data-release)
                                       &key
                                       &allow-other-keys)
  (setf (gethash (release-id self) *all-releases*) self))

;; sources

(defmethod initialize-instance :after ((self data-source)
                                       &key
                                       &allow-other-keys)
  (setf (gethash  (source-id self) *all-sources*) self))

;; tag-groups

(defmethod initialize-instance :after ((self data-tag-group)
                                       &key
                                       &allow-other-keys)
  (setf (gethash (tgroup-id self) *all-tag-groups*) self))

;; tags

(defmethod initialize-instance :after ((self data-tag)
                                       &key
                                       &allow-other-keys)
  (setf (gethash (tag-name self) *all-tags*) self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instantiating FRED® objects
;;;
;;; It's generally better to retrieve data as needed which will minimize
;;; the stress on the FRED servers, but if you need all categories and/or
;;; tags sooner or later anyway, these functions can be used to pre-load
;;; all such objects. If you do that, you might want to save that information
;;; locally so that the queries do not have to be repeated each time an
;;; app is run (the delay caused by doing this would be intolerable for users).

(defun initialize-fred-categories (&optional (depth-limit 9) (init-list '("0")))
  ;; The specified depth-limit default is sufficient (as of August 2014) to download
  ;; all categories. Since there are over 4,900 of these, this will take several
  ;; minutes to complete all necessary queries.
  (do* ((cats-to-init init-list)
        (cat-to-init (pop cats-to-init)
                     (pop cats-to-init))
        (cat (find-category cat-to-init)
             (find-category cat-to-init))
        (next-level-cats nil))
       ((null cat-to-init) (cond ((zerop depth-limit)
                                  t)
                                 ((null next-level-cats)
                                  depth-limit)
                                 (t
                                  (initialize-fred-categories (1- depth-limit) next-level-cats))))
    (unless cat
      (setf cat (make-instance 'fred-data-category :cat-id cat-to-init)))
    (setf next-level-cats (union next-level-cats (mapcar #'cat-id (cat-children cat)) :test #'string=))))

(defun initialize-fred-tags ()
  ;; If you want to use tags and tag-groups as part of your application, this can be called to initialize
  ;; all FRED® tags and tag groups. There are approximately 5,000 tags, so it will take a minute or two
  ;; for this to complete all necessary queries.
  (do* ((offset 0
                (+ offset 1000))
        (response (fred-tags :offset offset)
                  (fred-tags :offset offset)))
       ((null response) t)
    (dolist (tag-info response)
      (destructuring-bind (name group-id notes created popularity series-count) tag-info
        (or (find-tag name)
            (make-instance 'fred-data-tag
              :name name
              :group-id group-id
              :notes notes
              :created created
              :popularity popularity
              :series-count series-count))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class methods for fred-specific classes.
;;
;; initialize-instance :after methods will execute FRED queries as needed to
;; fill in slots that contain basic property values for that object if 
;; not provided by appropriate initargs.
;;
;; slot-unbound methods implement lazy evaluation of slots that are used
;; to contain lists of related fred classes.
;; If such a slot is unbound and the slot is accessed, then FRED queries
;; will be executed to fill in slot values.

;; fred-data-category

(defmethod initialize-instance :after ((self fred-data-category)
                                       &key
                                       &allow-other-keys)
  (with-slots (cat-id cat-name cat-parent cat-notes) self
    (unless (and cat-name cat-parent)
      (destructuring-bind (id name parent-id notes) (fred-category :category-id cat-id)
        (declare (ignore id))
        (setf cat-name name)
        (setf cat-notes notes)
        (setf cat-parent (or (find-category parent-id)
                             (make-instance 'fred-data-category :id parent-id)))))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-category)))
                         (self fred-data-category)
                         (slot (eql 'cat-children)))
  (setf (slot-value self 'cat-children) nil)
  (dolist (child-info (fred-category-children :category-id (cat-id self)))
    (destructuring-bind (id name parent-id notes) child-info
      (push (or (find-category id)
                (make-instance 'fred-data-category
                  :id id
                  :name name
                  :notes notes
                  :parent (or (find-category parent-id)
                              (make-instance 'fred-data-category :id parent-id))))
            (slot-value self 'cat-children))))
  (slot-value self 'cat-children))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-category)))
                         (self fred-data-category)
                         (slot (eql 'cat-series)))
  (setf (slot-value self 'cat-series) nil)
  (dolist (series-info (fred-category-series :category-id (cat-id self)))
    (destructuring-bind (id realtime-start realtime-end title observation-start observation-end
                            frequency frequency-short units units_short seasonal-adjustment
                            seasonal-adjustment-short last-updated popularity notes)
                        series-info
      (declare (ignore realtime-start realtime-end units_short seasonal-adjustment-short))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
                          :short-freq frequency-short
                          :units units
                          :seas-adj seasonal-adjustment
                          :last-update last-updated
                          :popularity popularity
                          :notes notes))))
        (push series (slot-value self 'cat-series)))))
  (slot-value self 'cat-series))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-category)))
                         (self fred-data-category)
                         (slot (eql 'cat-tags)))
  (setf (slot-value self 'cat-tags) nil)
  (dolist (tag-info (fred-category-tags :category-id (cat-id self)))
    (destructuring-bind (name group-id notes created popularity series-count) tag-info
      (let ((tag (or (find-tag name)
                     (make-instance 'fred-data-tag
                       :name name
                       :group-id group-id
                       :notes notes
                       :created created
                       :popularity popularity
                       :series-count series-count))))
        (push tag (slot-value self 'cat-tags)))))
  (slot-value self 'cat-tags))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-category)))
                         (self fred-data-category)
                         (slot (eql 'cat-related)))
  (setf (slot-value self 'cat-related) nil)
  (dolist (related-info (fred-category-related :category-id (cat-id self)))
    (destructuring-bind (name group-id notes created popularity series-count) related-info
      (let ((tag (or (find-tag name)
                     (make-instance 'fred-data-tag
                       :name name
                       :group-id group-id
                       :notes notes
                       :created created
                       :popularity popularity
                       :series-count series-count))))
        (push tag (slot-value self 'cat-related)))))
  (slot-value self 'cat-related))
      
;; fred-data-series

(defmethod initialize-instance :after ((self fred-data-series)
                                       &key
                                       &allow-other-keys)
  (with-slots (series-id series-title series-start-dt series-end-dt series-frequency
                         series-units series-seasonally-adj series-last-update-dt
                         series-popularity series-notes series-transform) self
    (unless series-title
      (destructuring-bind (id realtime-start realtime-end title observation-start 
                              observation-end frequency frequency-short units units-short
                              seasonal-adjustment seasonal-adjustment-short last-updated
                              popularity notes)
                          (fred-series :series-id series-id)
        (declare (ignore id realtime-start realtime-end frequency-short units-short
                         seasonal-adjustment))
        (setf series-title title)
        (setf series-start-dt observation-start)
        (setf series-end-dt observation-end)
        (setf series-frequency frequency)
        (setf series-units (concatenate 'string (transform-string series-transform) units))
        (setf series-seasonally-adj seasonal-adjustment-short)
        (setf series-last-update-dt last-updated)
        (setf series-popularity popularity)
        (setf series-notes notes)))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-series)))
                         (self fred-data-series)
                         (slot (eql 'series-categories)))
  (setf (slot-value self 'series-categories) nil)
  (dolist (category-info (fred-series-categories :series-id (series-id self)))
    (destructuring-bind (id name parent-id notes) category-info
      (let ((cat (or (find-category id)
                     (make-instance 'fred-data-category
                       :id id
                       :name name
                       :parent (or (find-category parent-id)
                                   (make-instance 'fred-data-category :id parent-id))
                       :notes notes))))
        (push cat (slot-value self 'series-categories)))))
  (slot-value self 'series-categories))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-series)))
                         (self fred-data-series)
                         (slot (eql 'series-release)))
  (destructuring-bind (id realtime-start realtime-end name press-release link notes)
                      (fred-series-release :series-id (series-id self))
    (declare (ignore realtime-start realtime-end))
    (setf (slot-value self 'series-release) 
          (or (find-release id)
              (make-instance 'fred-data-release
                :id id
                :name name
                :press-release press-release
                :link link
                :notes notes)))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-series)))
                         (self fred-data-series)
                         (slot (eql 'series-tags)))
  (setf (slot-value self 'series-tags) nil)
  (dolist (tag-info (fred-series-tags :series-id (series-id self)))
    (destructuring-bind (name group-id notes created popularity series-count) tag-info
      (let ((tag (or (find-tag name)
                     (make-instance 'fred-data-tag
                       :name name
                       :group-id group-id
                       :notes notes
                       :created created
                       :popularity popularity
                       :series-count series-count))))
        (push tag (slot-value self 'series-tags)))))
  (slot-value self 'series-tags))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-series)))
                         (self fred-data-series)
                         (slot (eql 'series-observations)))
  (let* ((obs-info (fred-series-observations :series-id (series-id self)
                                             :observation-start (series-start-dt self)
                                             :units (series-transform self)))
         (obs-values (mapcar #'fourth obs-info)))
    (set-series-observations self obs-values))
   (slot-value self 'series-observations))

(defmethod series-denominations ((self data-series))
  ;; Returns a list of all currency denominations found within the series-units text.
  ;; Returned list contains iso currency codes as keywords. See currency.lisp for codes.
  ;; Most series units will refer to a single denomination, but some series that represent
  ;; ratios may contain more than one.
  (currencies-within-string (series-units self)))

(defmethod series-multipliers ((self data-series))
  ;; Returns a list of all multipliers found within the series-units text.
  ;; Returned list contains integer multipliers as shown in code below.
  ;; Most series units will refer to a single multiplier, but some series that represent
  ;; ratios may contain more than one.
  (do ((wrds (words (series-units self)))
       (found-multipliers nil))
      ((null wrds) (nreverse found-multipliers))
    (let ((mult (some #'(lambda (wrd)
                          (rest (assoc wrd 
                                       '(("ones" . 1) ("one" . 1)
                                         ("tens" . 10) ("ten" . 10)
                                         ("hundreds" . 100) ("hundred" . 100)
                                         ("thousands" . 1000) ("thousand" . 1000)
                                         ("millions" . 1000000) ("million" . 1000000)
                                         ("billions" . 1000000000) ("billion" . 1000000000)
                                         ("trillions" . 1000000000000) ("trillion" . 1000000000000)
                                         ("percent" . 100) ("percentage" . 100))
                                       :test #'string-equal)))
                      wrds)))
      (when mult
        (push mult found-multipliers))
      (setf wrds (rest wrds)))))

;; data-series

(defmethod series-observation ((self data-series) obs-date)
  ;; Recall that period values are associated with the first date within the period.
  ;; In most cases what we want to return is a value that represents the period of time
  ;; within which the obs-data resides. This would be the value that represents
  ;; the nearest prior date within the data array.
  ;; series-interpolation-method = :current
  ;;
  ;; Sometimes we would like to use the value at the end of the prior period.
  ;; series-interpolation-method = :prior
  ;;
  ;; Sometimes we would like to use the prior period value if the date is closer to
  ;; the beginning of the perior than the end or the current period value if the date
  ;; is closer to the beginning of the next period.
  ;; series-interpolation-method = :closest
  ;;
  ;; Sometimes we would like to interpolate between the value of the prior
  ;; period and the ending value for period within which the date resides.
  ;; series-interpolation-method = :average
  (with-slots (series-id series-start-dt series-end-dt series-frequency
               series-interpolation-method) self
    (when (or (< obs-date series-start-dt)
              (> obs-date series-end-dt))
      (error "~a is not within the range of observed dates for series ~s"
             (fred-date-string obs-date)
             series-id))
    (multiple-value-bind (prior-period-dt prior-period-indx current-period-dt current-period-indx next-period-dt)
                         (period-indices series-start-dt series-frequency obs-date)
      (declare (ignore prior-period-dt))
      (let ((prior-val (aref (series-observations self) prior-period-indx))
            (current-val (aref (series-observations self) current-period-indx)))
        (case series-interpolation-method
          (:current
           current-val)
          (:prior
           prior-val)
          (:closest
           (if (< (-days obs-date current-period-dt) (-days next-period-dt obs-date))
             prior-val
             current-val))
          (:avg ;; interpolate based on the fraction of the period until obs-date
           (+ prior-val (* (- current-val prior-val)
                           (/ (-days obs-date current-period-dt) (-days next-period-dt current-period-dt))))))))))

;; derived-data-series

(defmethod initialize-instance :after ((self derived-data-series)
                                       &key id args freq units all start end
                                       &allow-other-keys)
  (unless id
    (setf (series-id self)
          (do* ((arg-ids (mapcar #'series-id args))
                (indx 0
                      (1+ indx))
                (new-id (format nil "DDS~{_~a~}" arg-ids)
                        (format nil "DDS~{_~a~}-~s" arg-ids indx)))
               ((not (gethash new-id *all-series*)) new-id))))
  (unless freq
    (setf (series-frequency self) (series-frequency (first args))))
  (unless units
    (setf (series-units self) (series-units (first args))))
  (unless (plusp start)
    (setf (series-start-dt self) (apply #'max (mapcar #'series-start-dt args))))
  (unless (plusp end)
    (setf (series-end-dt self) (apply #'min (mapcar #'series-end-dt args))))
  (when (and (not (slot-boundp self 'series-observations)) all)
    ;; transform all series observations at init time
    ;; transform-func will be applied to all args (which are data-series) directly
    (set-series-observations self 
                             (apply (dds-transform-func self) 
                                    (dds-transform-args self)))))

(defmethod series-observation ((self derived-data-series) obs-date)
  (if (and (slot-boundp self 'series-observations) (series-observations self))
      (call-next-method self obs-date)
      (apply (dds-transform-func self) (mapcar #'(lambda (arg)
                                                   (series-observation arg obs-date))
                                               (dds-transform-args self)))))

;; fred-data-release

(defmethod initialize-instance :after ((self fred-data-release) 
                                       &key
                                       &allow-other-keys)
  (with-slots (release-id release-name release-press-release release-link release-notes) self
    (unless release-name
      (destructuring-bind (id realtime-start realtime-end name press-release link notes)
                          (fred-release :release-id release-id)
        (declare (ignore id realtime-start realtime-end))
        (setf release-name name)
        (setf release-press-release press-release)
        (setf release-link link)
        (setf release-notes notes)))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-release)))
                         (self fred-data-release)
                         (slot (eql 'release-dates)))
  (setf (release-dates self)
        (mapcar #'first (fred-release-dates :release-id (release-id self)))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-release)))
                         (self fred-data-release)
                         (slot (eql 'release-series)))
  (setf (release-series self) nil)
  (dolist (series-info (fred-release-series :release-id (release-id self)))
    (destructuring-bind (id realtime-start realtime-end title observation-start observation-end
                            frequency frequency-short units units_short seasonal-adjustment
                            seasonal-adjustment-short last-updated popularity notes)
                        series-info
      (declare (ignore realtime-start realtime-end units_short seasonal-adjustment))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
                          :short-freq frequency-short
                          :units units
                          :seas-adj seasonal-adjustment-short
                          :last-update last-updated
                          :popularity popularity
                          :notes notes))))
        (push series (slot-value self 'release-series)))))
  (slot-value self 'release-series))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-release)))
                         (self fred-data-release)
                         (slot (eql 'release-sources)))
  (setf (release-sources self) nil)
  (dolist (source-info (fred-release-sources :release-id (release-id self)))
    (destructuring-bind (id realtime-start realtime-end name link notes)
                        source-info
      (declare (ignore realtime-start realtime-end))
      (let ((source (or (find-source id)
                        (make-instance 'fred-data-source
                          :id id
                          :name name
                          :link link
                          :notes notes))))
        (push source (slot-value self 'release-sources)))))
  (slot-value self 'release-sources))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-release)))
                         (self fred-data-release)
                         (slot (eql 'release-tags)))
  (setf (release-tags self) nil)
  (dolist (name (mapcar #'first (fred-release-tags :release-id (release-id self))))
    (let  ((tag (or (find-tag name)
                    (make-instance 'fred-data-tag
                      ;; we don't use all info here because the series-count returned
                      ;; by this query is the number of series associated with release
                      ;; which contain the tag rather than the total number of series
                      ;; which contain the tag. So we'll let the initialize-instance
                      ;; method make another query that returns the correct value.
                      :name name))))
      (push tag (slot-value self 'release-tags))))
  (slot-value self 'release-tags))

;; fred-data-source

(defmethod initialize-instance :after ((self fred-data-source) 
                                       &key
                                       &allow-other-keys)
  (with-slots (source-id source-name source-link source-notes) self
    (unless source-name
      (destructuring-bind (id realtime-start realtime-end name link notes)
                          (fred-source :source-id (source-id self))
        (declare (ignore id realtime-start realtime-end))
        (setf source-name name)
        (setf source-link link)
        (setf source-notes notes)))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-source)))
                         (self fred-data-source)
                         (slot (eql 'source-releases)))
  (setf (slot-value self 'source-releases) nil)
  (dolist (release-info (fred-source-releases :source-id (source-id self)))
    (destructuring-bind (id realtime-start realtime-end name press-release link notes)
                        release-info
      (declare (ignore realtime-start realtime-end))
      (let ((release (or (find-release id)
                         (make-instance 'fred-data-release
                           :id id
                           :name name
                           :press-release press-release
                           :link link
                           :notes notes))))
        (push release (slot-value self 'source-releases)))))
  (slot-value self 'source-releases))

;; fred-data-tag

(defmethod initialize-instance :after ((self fred-data-tag) 
                                       &key
                                       &allow-other-keys)
  (with-slots (tag-name tag-group-id tag-notes tag-created tag-popularity tag-series-count tag-group) self
    (unless tag-group-id
      (destructuring-bind (name group-id notes created popularity series-count)
                          (first (fred-tags :tag-names (list tag-name)))
        (declare (ignore name))
        (setf tag-group-id group-id)
        (setf tag-notes notes)
        (setf tag-created created)
        (setf tag-popularity popularity)
        (setf tag-series-count series-count)))
    (setf tag-group (or (find-tag-group tag-group-id)
                        (make-instance 'fred-data-tag-group :id tag-group-id)))
    (push self (tgroup-tags tag-group))))

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-tag)))
                         (self fred-data-tag)
                         (slot (eql 'tag-series)))
  (setf (slot-value self 'tag-series) nil)
  (dolist (series-info (fred-tags-series :tag-names (list (tag-name self))))
    (destructuring-bind (id realtime-start realtime-end title observation-start observation-end
                            frequency frequency-short units units_short seasonal-adjustment
                            seasonal-adjustment-short last-updated popularity notes)
                        series-info
      (declare (ignore realtime-start realtime-end units_short seasonal-adjustment))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
                          :short-freq frequency-short
                          :units units
                          :seas-adj seasonal-adjustment-short
                          :last-update last-updated
                          :popularity popularity
                          :notes notes))))
        (push series (slot-value self 'tag-series)))))
  (slot-value self 'tag-series))

;; fred-data-tag-group

(defmethod slot-unbound ((cl (eql (find-class 'fred-data-tag-group)))
                         (self fred-data-tag-group)
                         (slot (eql 'tgroup-tags)))
  (setf (tgroup-tags self) nil)
  (dolist (tag-info (fred-tags :tag-group-id (tgroup-id self)))
    (destructuring-bind (name group-id notes created popularity series-count)
                        tag-info
      (let  ((tag (or (find-tag name)
                      (make-instance 'fred-data-tag
                        :name name
                        :group-id group-id
                        :notes notes
                        :created created
                        :popularity popularity
                        :series-count series-count))))
        (push tag (slot-value self 'tgroup-tags)))))
  (slot-value self 'tgroup-tags))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRED API functions

;; Query and response manipulation functions

#|
The xml parser being used will transform:

<token>
</token>

into 

(:|token| "
")

i.e. a list that contains as its second element a string containing a single 
linefeed character. That might be valid for some sorts of XML structures, but for
what we get from FRED we will just remove those strings.
 
|#

(defun fred-query (query-string key-value-alist)
  (or *fred-api-key*
      (initialize-fred)
      (error "FRED API key not initialized. Call initialize-fred with api key as the argument or\
 create API_Key.txt containing key as quoted string in home directory"))
  (let* ((result (parse-xml-string 
                  (http-request (concatenate 'string "https://api.stlouisfed.org/fred/" query-string)
                                :parameters (acons "api_key" *fred-api-key* key-value-alist))))
         (first-obj (xml-form-tag result))
         (error-info  (and (listp first-obj)
                           (eq (xml-form-tag first-obj) :|error|)
                           (fifth first-obj))))
    (when error-info
      (error error-info))
    (remove-lf-strs result)))

(defun fred-string-to-num (str)
  ;; It appears that fred sometimes returns "." for values if it doesn't have a number to return
  ;; We will convert that to 0 in the absence of anything better
  (if (string= str ".")
      0.0
      (read-from-string str nil 0)))

(defun fred-int-string (int)
  (typecase int
    (string
     int)
    (integer
     (format nil "~s" int))
    (t
     (error "~s is not a valid integer argument" int))))

(defun param-string (ps)
  ;; Turns ps into a string if it is a keyword symbol
  (typecase ps
    (string ps)
    (keyword (substitute #\_ #\- (string-downcase (symbol-name ps))))
    (boolean (if ps "true" "false"))
    (t (error "~s is not a valid keyword or string" ps))))

(defun fred-date-string (dt)
  (typecase dt
    (string
     dt)
    (integer
     (string-intl-date dt))
    (t
     (error "~s is not a valid date argument" dt))))

(defun units-string (units)
  ;; lin = no transformation, 
  ;; chg = change from previous value
  ;; ch1 = change from year ago
  ;; pch = % change from previous value
  ;; pc1 = % change from year ago
  ;; pca = compounded annual rate of change
  ;; cch = continuously compounded rate of change
  ;; cca = continuously compounded annual rate of change
  ;; log = natural log
  ;; see http://alfred.stlouisfed.org/help#growth_formulas for unit transformation formulas
  (or (find (param-string units)
            '("lin" "chg" "ch1" "pch" "pc1" "pca" "cch" "cca" "log")
            :test #'string=)
      (error "~s is not a valid units keyword or string" units)))

(defun transform-string (transform-key)
  (or (rest (assoc transform-key
                   '((:lin . "")
                     (:chg . "change of ")
                     (:ch1 . "change from previous year of ")
                     (:pch . "% change of ")
                     (:pc1 . "% change from previous year of ")
                     (:pca . "compounded annual rate of change of ")
                     (:cch . "continuously compounded rate of change of ")
                     (:cca . "continuously compounded annual rate of change of ")
                     (:log . "natural log of "))))
      ""))
           
(defun search-type-string (stype)  
  (or (find (param-string stype) '("full_text" "series_id") :test #'string=)
      (error "~s is not a valid search-type keyword or string" stype)))

(defun series-order-by-string (ob)
  (or (find (param-string ob) 
            '("search_rank" "series_id" "title" "units" "frequency" "seasonal_adjustment"
              "realime_start" "realtime_end" "last_updated" "observation_start"
              "observation_end" "popularity")
            :test #'string=)
      (error "~s is not a valid search-type keyword or string" ob)))

(defun tag-order-by-string (ob)
  (or (find (param-string ob)
            '("series_count" "popularity" "created" "name" "group_id")
            :test #'string=)
      (error "~s is not a valid tag-order keyword or string" ob)))

(defun release-order-by-string (ob)
  (or (find (param-string ob)
            '("release_id" "name" "press_release" "realtime_start" "realtime_end")
            :test #'string=)
      (error "~s is not a valid release-order keyword or string" ob)))
  
(defun sort-order-string (so)
  ;; asc = ascending
  ;; desc = descending
  (or (find (param-string so) '("asc" "desc") :test #'string=)
      (error "~s is not a valid sort-order keyword or string" so)))

(defun frequency-string (freq)
  ;; Must be a string or keyword from the list below.
  ;; d = daily, w = weekly, bw = bi-weekly, m = monthly, q = quarterly, sa = semi-annually, a = annually
  ;; weX = week ending on day X, bwX = bi-weekly ending on day X.
  (or (find (param-string freq)
            '("d" "w" "bw" "m" "q" "sa" "a" "wef" "weth" "wew" "wetu" "wem" "wesu" "wesa" "bwew" "bwem")
            :test #'string=)
      (error "~s is not a valid frequency string or keyword" freq)))

(defun aggregation-string (agg)
  ;; Must be a string or keyword from the list below
  ;; avg = average, sum = sum, eop = end of period
   (or (find (param-string agg)
            '("avg" "sum" "eop")
            :test #'string=)
      (error "~s is not a valid aggregation method string or keyword" agg)))

(defun filter-var-string (fv)
  (or (find (param-string fv)
            '("frequency" "units" "seasonal_adjustment")
            :test #'string=)
      (error "~s is not a valid filter-variable keyword or string" fv)))

(defun tag-group-id-string (tg)
  ;; freq = frequency, gen = general or concept, geo = geography, geot = geography type
  ;; rls = release, seas = seasonal adjustment, src = source 
  (or (find (param-string tg)
            '("freq" "gen" "geo" "geot" "rls" "seas" "src")
            :test #'string=)
      (error "~s is not a valid tag-group-id keyword or string" tg)))

(defun geo-string (gs)
  (or (find (param-string gs)
            '("macro" "regional" "all")
            :test #'string=)
      (error "~s is not a valid geographic parameter string or keyword" gs)))

(defun list-non-nil (&rest items)
  (delete nil items))

(defun xml-assoc (key xml-struct)
  ;; Return value immediately following key in xml-struct
  ;; If not found return an empty string which can maybe be translated into some reasonable default
  (let ((pos (position key xml-struct)))
    (if pos 
      (nth (1+ pos) xml-struct)
      "")))

;; Utility functions for parsing xml forms that represent returned FRED data

(setf (symbol-function 'xml-form-tag) #'first)
(setf (symbol-function 'xml-form-body) #'rest)

(defmacro parse-body-tags (xml-form parse-func)
  ;; xml-form is a response from a FRED query that is known to provide
  ;; a list of xml forms as the body of the response. In most cases the
  ;; tag of that form is itself a form that provides the information we
  ;; are looking to extract and this function returns a list of tuples
  ;; (lists) that contain the data extracted by the parse-func provided.
  `(mapcar #'(lambda (body-form)
               (,parse-func (xml-form-tag body-form)))
           (xml-form-body ,xml-form)))

(defmacro parse-body (xml-form parse-func)
  ;; xml-form is a response from a FRED query that is known to provide
  ;; a list of xml forms as the body of the response. The parse-func
  ;; is called on each body form of the xml-form
  `(mapcar (function ,parse-func) (xml-form-body ,xml-form)))

(defun parse-category-tag (tag)
  ;; Returns a list: (id name parent-id notes)
   (list (xml-assoc :|id| tag)
         (xml-assoc :|name| tag)
         (xml-assoc :|parent_id| tag)
         (xml-assoc :|notes| tag)))

(defun parse-series-tag (tag)
  ;; Returns a list: 
  ;; (id realtime-start realtime-end title observation-start observation-end frequency
  ;;   frequency-short units units-short seasonal-adjustment seasonal-adjustment-short
  ;;   last-updated popularity notes)
  (list (xml-assoc :|id| tag)
        (intl-string-to-date (xml-assoc :|realtime_start| tag))
        (intl-string-to-date (xml-assoc :|realtime_end| tag))
        (xml-assoc :|title| tag)
        (intl-string-to-date (xml-assoc :|observation_start| tag))
        (intl-string-to-date (xml-assoc :|observation_end| tag))
        (fred-string-to-freq-key (xml-assoc :|frequency| tag))
        (fred-string-to-key (xml-assoc :|frequency_short| tag))
        (xml-assoc :|units| tag)
        (xml-assoc :|units_short| tag)
        (xml-assoc :|seasonal_adjustment| tag)
        (fred-string-to-key (xml-assoc :|seasonal_adjustment_short| tag))
        (intl-string-to-date (xml-assoc :|last_updated| tag))
        (fred-string-to-num (xml-assoc :|popularity| tag))
        (xml-assoc :|notes| tag)))

(defun parse-tag-tag (tag)
  ;; Returns a list: 
  ;; (name group-id notes created popularity series-count)
   (list (xml-assoc :|name| tag)
         (xml-assoc :|group_id| tag)
         (xml-assoc :|notes| tag)
         (intl-string-to-date (xml-assoc :|created| tag))
         (fred-string-to-num (xml-assoc :|popularity| tag))
         (fred-string-to-num (xml-assoc :|series_count| tag))))

(defun parse-release-tag (tag)
  ;; Returns a list: 
  ;; (id realtime-start realtime-end name press-release link notes)
  (list (xml-assoc :|id| tag)
        (intl-string-to-date (xml-assoc :|realtime_start| tag))
        (intl-string-to-date (xml-assoc :|realtime_end| tag))
        (xml-assoc :|name| tag)
        (xml-assoc :|press_release| tag)
        (xml-assoc :|link| tag)
        (xml-assoc :|notes| tag)))

(defun parse-release-date-form (xml-form)
  ;; Returns a list: 
  ;; (date release-id release-name release-last-updated)
  ;; release-last-updated will be nil unless query asked for dates for which no data was released
  (let ((tag (xml-form-tag xml-form)))
    (list (intl-string-to-date (first (xml-form-body xml-form)))
          (xml-assoc :|release_id| tag)
          (xml-assoc :|release_name| tag)
          (xml-assoc :|release-last-updated| tag))))

(defun parse-vintage-date-form (xml-form)
  ;; Returns a single lisp date value
  (intl-string-to-date (first (xml-form-body xml-form))))

(defun parse-source-tag (tag)
  ;; Returns a list: 
  ;; (id realtime-start realtime-end name link notes)
  (list (xml-assoc :|id| tag)
        (intl-string-to-date (xml-assoc :|realtime_start| tag))
        (intl-string-to-date (xml-assoc :|realtime_end| tag))
        (xml-assoc :|name| tag)
        (xml-assoc :|link| tag)
        (xml-assoc :|notes| tag)))

(defun parse-observation-tag (tag)
  ;; Returns a list: 
  ;; (realtime-start realtime-end date value)
  (list (intl-string-to-date (xml-assoc :|realtime_start| tag))
        (intl-string-to-date (xml-assoc :|realtime_end| tag))
        (intl-string-to-date (xml-assoc :|date| tag))
        (fred-string-to-num (xml-assoc :|value| tag))))

;; Below are the actual FRED queries and transformation of response into a list of values.
;; Probably the best way to use these functions is by calling them as the expression evaluated 
;; to determine parameter values for a destructuring-bind call. See examples elsewhere in this file.
;; The query parameters are passed back in the tag of the xml form returned by all FRED api functions.
;; Those are typically not needed, but are passed back (as a lisp list) as the second
;; value returned from each fred-... function in case some developer wishes to know what default
;; parameters were used by the query.

;; Category Functions

(defun fred-category (&key (category-id "0"))
  ;; fred/category - Get a category.
  (let ((response (fred-query "category" (list (cons "category_id" category-id)))))
    (values (parse-category-tag (xml-form-tag (second response)))
            (xml-form-tag response))))

(defun fred-category-children (&key (category-id "0")
                                    (realtime-start nil)
                                    (realtime-end nil))
  ;; fred/category/children - Get the child categories for a specified parent category.
  (let* ((category-id-cons
          (cons "category_id" category-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "category/children"
                               (list-non-nil category-id-cons
                                             rt-start-cons
                                             rt-end-cons))))
    (values (parse-body-tags response parse-category-tag)
            (xml-form-tag response))))

(defun fred-category-related (&key (category-id "0")
                                   (realtime-start nil)
                                   (realtime-end nil))
  ;; fred/category/related - Get the related categories for a category.
  (let* ((category-id-cons
          (cons "category_id" category-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "category/related"
                               (list-non-nil category-id-cons
                                             rt-start-cons
                                             rt-end-cons))))
    (values (parse-body-tags response parse-category-tag)
            (xml-form-tag response))))

(defun fred-category-series (&key (category-id "0")
                                  (realtime-start nil)
                                  (realtime-end nil)
                                  (limit nil)
                                  (offset nil)
                                  (order-by nil)
                                  (sort-order nil)
                                  (filter-variable nil)
                                  (filter-value nil)
                                  (tag-names nil))
  ;; fred/category/series - Get the series in a category.
  (let* ((category-id-cons (cons "category_id" category-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (series-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (fvar-cons (and filter-variable
                         (cons "filter_variable" (filter-var-string filter-variable))))
         (fval-cons (and filter-value
                         (cons "filter_value" (param-string filter-value))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (response (fred-query "category/series"
                               (list-non-nil category-id-cons
                                             rt-start-cons
                                             rt-end-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons
                                             fvar-cons
                                             fval-cons
                                             tag-names-cons))))
    (values (parse-body-tags response parse-series-tag)
            (xml-form-tag response))))

(defun fred-category-tags (&key (category-id "0")
                                (realtime-start nil)
                                (realtime-end nil)
                                (tag-names nil)
                                (tag-group-id nil)
                                (search-text nil)
                                (limit nil)
                                (offset nil)
                                (order-by nil)
                                (sort-order nil))
  ;; fred/category/tags - Get the tags for a category.
  (let* ((category-id-cons (cons "category_id" category-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "category/tags"
                               (list-non-nil category-id-cons
                                             rt-start-cons
                                             rt-end-cons
                                             tag-names-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

(defun fred-category-related-tags (&key (category-id "0")
                                        (tag-names '("annual"))
                                        (realtime-start nil)
                                        (realtime-end nil)
                                        (tag-group-id nil)
                                        (search-text nil)
                                        (limit nil)
                                        (offset nil)
                                        (order-by nil)
                                        (sort-order nil))
  ;; fred/category/related_tags - Get the related tags for a category.
  ;; Find all tags that are associated with any series in the category that also contains
  ;; all of the tags specified in the tag-names parameter.
  (let* ((category-id-cons (cons "category_id" category-id))
         (tag-names-cons (cons "tag_names" (conjoin-strings-for-query tag-names)))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "category/related_tags"
                               (list-non-nil category-id-cons
                                             tag-names-cons
                                             rt-start-cons
                                             rt-end-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

;; Release Functions

(defun fred-releases ()
  ;; fred/releases - Get all releases of economic data.
  (let ((response (fred-query "releases" nil)))
    (values (parse-body-tags response parse-release-tag)
            (xml-form-tag response))))

(defun fred-releases-dates (&key (realtime-start nil)
                                 (realtime-end nil)
                                 (limit nil)
                                 (offset nil)
                                 (order-by nil)
                                 (sort-order nil)
                                 (include-release-dates-with-no-data nil ird-provided))
  ;; fred/releases/dates - Get release dates for all releases of economic data.
  (let* ((rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (irdwnd-cons (if ird-provided ;; need this because nil is a valid value for this parameter
                          (cons "include_release_dates_with_no_data" (param-string include-release-dates-with-no-data))))
         (response (fred-query "releases/dates"
                               (list-non-nil rts-cons 
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons
                                             irdwnd-cons))))
    (values (parse-body response parse-release-date-form)
            (xml-form-tag response))))

(defun fred-release (&key (release-id "0")
                          (realtime-start nil)
                          (realtime-end nil))
  ;; fred/release - Get a release of economic data.
  (let* ((release-id-cons (cons "release_id" release-id))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "release"
                               (list-non-nil release-id-cons
                                             rts-cons
                                             rte-cons))))
    (values (parse-release-tag (xml-form-tag (second response))) ;; second is short-hand for (first (xml-form-body ...))
            (xml-form-tag response))))

(defun fred-release-dates (&key (release-id "0")
                                (realtime-start nil)
                                (realtime-end nil)
                                (limit nil)
                                (offset nil)
                                (order-by nil)
                                (sort-order nil)
                                (include-release-dates-with-no-data nil ird-provided))
  ;; fred/release/dates - Get release dates for a release of economic data.
  (let* ((release-id-cons (cons "release_id" release-id))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (irdwnd-cons (if ird-provided
                          (cons "include_release_dates_with_no_data" (param-string include-release-dates-with-no-data))))
         (response (fred-query "release/dates"
                               (list-non-nil release-id-cons
                                             rts-cons 
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons
                                             irdwnd-cons))))
    (values (parse-body response parse-release-date-form)
            (xml-form-tag response))))

(defun fred-release-series (&key (release-id "0")
                                 (realtime-start nil)
                                 (realtime-end nil)
                                 (limit nil)
                                 (offset nil)
                                 (order-by nil)
                                 (sort-order nil)
                                 (filter-variable nil)
                                 (filter-value nil)
                                 (tag-names nil))
  ;; fred/release/series - Get the series on a release of economic data.
  (let* ((release-id-cons (cons "release_id" release-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (series-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (fvar-cons (and filter-variable
                         (cons "filter_variable" (filter-var-string filter-variable))))
         (fval-cons (and filter-value
                         (cons "filter_value" (param-string filter-value))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (response (fred-query "release/series"
                               (list-non-nil release-id-cons
                                             rt-start-cons
                                             rt-end-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons
                                             fvar-cons
                                             fval-cons
                                             tag-names-cons))))
    (values (parse-body-tags response parse-series-tag)
            (xml-form-tag response))))
  
(defun fred-release-sources (&key (release-id "0")
                                  (realtime-start nil)
                                  (realtime-end nil))
  ;; fred/release/sources - Get the sources for a release of economic data.
  (let* ((release-id-cons (cons "release_id" release-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "release/sources"
                               (list-non-nil release-id-cons
                                             rt-start-cons
                                             rt-end-cons))))
    (values (parse-body-tags response parse-source-tag)
            (xml-form-tag response))))

(defun fred-release-tags (&key (release-id "0")
                               (realtime-start nil)
                               (realtime-end nil)
                               (tag-names nil)
                               (tag-group-id nil)
                               (search-text nil)
                               (limit nil)
                               (offset nil)
                               (order-by nil)
                               (sort-order nil))
  ;; fred/release/tags - Get the tags for a release.
  ;; Be careful about the series-count value that is returned for each tag. It represents the number
  ;; of SERIES ASSOCIATED WITH THIS RELEASE that have that tag (rather than the overall number of
  ;; series that contain the tag.
  (let* ((release-id-cons (cons "release_id" release-id))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "release/tags"
                               (list-non-nil release-id-cons
                                             rt-start-cons
                                             rt-end-cons
                                             tag-names-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

(defun fred-release-related-tags (&key (release-id "0")
                                       (tag-names '("annual"))
                                       (realtime-start nil)
                                       (realtime-end nil)
                                       (tag-group-id nil)
                                       (search-text nil)
                                       (limit nil)
                                       (offset nil)
                                       (order-by nil)
                                       (sort-order nil))
  ;; fred/release/related_tags - Get the related tags for a release.
  (let* ((release-id-cons (cons "release_id" release-id))
         (tag-names-cons (cons "tag_names" (conjoin-strings-for-query tag-names)))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "release/related_tags"
                               (list-non-nil release-id-cons
                                             tag-names-cons
                                             rt-start-cons
                                             rt-end-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

;; Series Functions

(defun fred-series (&key series-id
                         (realtime-start nil)
                         (realtime-end nil))
  ;; fred/series - Get an economic data series.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "series"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons))))
    (values (parse-series-tag (xml-form-tag (second response)))
            (xml-form-tag response))))

(defun fred-series-categories (&key series-id
                                    (realtime-start nil)
                                    (realtime-end nil))
  ;; fred/series/categories - Get the categories for an economic data series.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "series/categories"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons))))
    (values (parse-body-tags response parse-category-tag)
            (xml-form-tag response))))

(defun fred-series-observations (&key series-id
                                      (realtime-start nil)
                                      (realtime-end nil)
                                      (limit nil)
                                      (offset nil)
                                      (sort-order nil)
                                      (observation-start nil)
                                      (observation-end nil)
                                      (units nil)
                                      (frequency nil)
                                      (aggregation-method nil)
                                      (output-type nil)
                                      (vintage-dates nil))
  ;; fred/series/observations - Get the observations or data values for an economic data series.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (obss-cons (and observation-start
                         (cons "observation_start" (fred-date-string observation-start))))
         (obse-cons (and observation-end
                         (cons "observation_end" (fred-date-string observation-end))))
         (units-cons (and units
                          (cons "units" (units-string units))))
         (freq-cons (and frequency
                         (cons "frequency" (frequency-string frequency))))
         (agg-cons (and aggregation-method
                        (cons "aggregation_method" (aggregation-string aggregation-method))))
         (out-cons (and output-type
                        (cons "output_type" (fred-int-string output-type))))
         (vdts-cons (and vintage-dates
                         (cons "vintage_dates" (conjoin-dates-for-query vintage-dates))))
         (response (fred-query "series/observations"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             sorder-cons
                                             obss-cons
                                             obse-cons
                                             units-cons
                                             freq-cons
                                             agg-cons
                                             out-cons
                                             vdts-cons))))
    (values (parse-body-tags response parse-observation-tag)
            (xml-form-tag response))))

(defun fred-series-release (&key series-id
                                 (realtime-start nil)
                                 (realtime-end nil))
  ;; fred/series/release - Get the release for an economic data series.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "series/release"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons))))
    (values (parse-release-tag (xml-form-tag (second response)))
            (xml-form-tag response))))

(defun fred-series-search (&key (search-text nil) ;; list of strings to search for
                                (search-type nil)
                                (realtime-start nil)
                                (realtime-end nil)
                                (limit nil)
                                (offset nil)
                                (order-by nil)
                                (sort-order nil)
                                (filter-variable nil)
                                (filter-value nil)
                                (tag-names nil))
  ;; fred/series/search - Get economic data series that match keywords.
  (let* ((stext-cons (cons "search_text" (conjoin-search-strings search-text)))
         (stype-cons (and search-type
                          (cons "search-type" (search-type-string search-type))))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (series-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (fvar-cons (and filter-variable
                         (cons "filter_variable" (filter-var-string filter-variable))))
         (fval-cons (and filter-value
                         (cons "filter_value" (param-string filter-value))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (response (fred-query "series/search"
                               (list-non-nil stext-cons 
                                             stype-cons
                                             rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons
                                             fvar-cons
                                             fval-cons
                                             tag-names-cons))))
    (values (parse-body-tags response parse-series-tag)
            (xml-form-tag response))))

(defun fred-series-search-tags (&key (series-search-text nil)  ;; list of strings to search for
                                     (realtime-start nil)
                                     (realtime-end nil)
                                     (tag-names nil)
                                     (tag-group-id nil)
                                     (tag-search-text nil)
                                     (limit nil)
                                     (offset nil)
                                     (order-by nil)
                                     (sort-order nil))
  ;; fred/series/search/tags - Get the tags for a series search.
  (let* ((stext-cons (and series-search-text
                          (cons "series_search_text" (conjoin-search-strings series-search-text))))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (tag-stext-cons (and tag-search-text
                              (cons "search_text" (conjoin-search-strings tag-search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "series/search/tags"
                               (list-non-nil stext-cons
                                             rts-cons
                                             rte-cons
                                             tag-names-cons
                                             tag-group-id-cons
                                             tag-stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

(defun fred-series-search-related-tags (&key (series-search-text nil) ;; list of strings to search for
                                             (realtime-start nil)
                                             (realtime-end nil)
                                             (tag-names nil)
                                             (tag-group-id nil)
                                             (tag-search-text nil)
                                             (limit nil)
                                             (offset nil)
                                             (order-by nil)
                                             (sort-order nil))
  ;; fred/series/search/related_tags - Get the related tags for a series search.
  (let* ((stext-cons (and series-search-text
                          (cons "series_search_text" (conjoin-search-strings series-search-text))))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (tag-stext-cons (and tag-search-text
                              (cons "search_text" (conjoin-search-strings tag-search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "series/search/related_tags"
                               (list-non-nil stext-cons
                                             rts-cons
                                             rte-cons
                                             tag-names-cons
                                             tag-group-id-cons
                                             tag-stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

(defun fred-series-tags (&key series-id
                              (realtime-start nil)
                              (realtime-end nil)
                              (order-by nil)
                              (sort-order nil))
  ;; fred/series/tags - Get the tags for an economic data series.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "series/tags"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
            (xml-form-tag response))))

(defun fred-series-updates (&key (realtime-start nil)
                                 (realtime-end nil)
                                 (limit nil)
                                 (offset nil)
                                 (filter-value nil))
  ;; fred/series/updates - Get economic data series sorted by when observations were 
  ;; updated on the FRED® server.
  (let* ((rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (fval-cons (and filter-value
                         (cons "filter_value" (geo-string filter-value))))
         (response (fred-query "series/updates"
                               (list-non-nil rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             fval-cons))))
    (values (parse-body-tags response parse-series-tag)
            (xml-form-tag response))))

(defun fred-series-vintagedates (&key series-id
                                      (realtime-start nil)
                                      (realtime-end nil)
                                      (limit nil)
                                      (offset nil)
                                      (sort-order nil))
  ;; fred/series/vintagedates - Get the dates in history when a series' data values 
  ;; were revised or new data values were released.
  (let* ((series-id-cons (cons "series_id" (string series-id)))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "series/vintagedates"
                               (list-non-nil series-id-cons
                                             rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             sorder-cons))))
    (values (parse-body response parse-vintage-date-form)
            (xml-form-tag response))))

;; Sources Functions

(defun fred-sources (&key (realtime-start nil)
                          (realtime-end nil)
                          (limit nil)
                          (offset nil)
                          (order-by nil)
                          (sort-order nil))
  ;; fred/sources - Get all sources of economic data.
  (let* ((rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (order-by-cons (and order-by
                             (cons "order_by" (series-order-by-string order-by))))
         (response (fred-query "sources"
                               (list-non-nil rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             sorder-cons
                                             order-by-cons))))
    (values (parse-body-tags response parse-source-tag)
            (xml-form-tag response))))

(defun fred-source (&key (source-id "1")
                         (realtime-start nil)
                         (realtime-end nil))
  ;; fred/source - Get a source of economic data.
  (let* ((source-id-cons (cons "source_id" source-id))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (response (fred-query "source"
                               (list-non-nil source-id-cons
                                             rts-cons
                                             rte-cons))))
    (values (parse-source-tag (xml-form-tag (second response)))
            (xml-form-tag response))))

(defun fred-source-releases (&key (source-id "1")
                                  (realtime-start nil)
                                  (realtime-end nil)
                                  (limit nil)
                                  (offset nil)
                                  (order-by nil)
                                  (sort-order nil))
  ;; fred/source/releases - Get the releases for a source.
  (let* ((source-id-cons (cons "source_id" source-id))
         (rts-cons (and realtime-start
                        (cons "realtime_start" (fred-date-string realtime-start))))
         (rte-cons (and realtime-end
                        (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (release-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "source/releases"
                               (list-non-nil source-id-cons
                                             rts-cons
                                             rte-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
     (values (parse-body-tags response parse-release-tag)
             (xml-form-tag response))))

;; Tags Functions

(defun fred-tags (&key (realtime-start nil)
                       (realtime-end nil)
                       (tag-names nil)
                       (tag-group-id nil)
                       (search-text nil)
                       (limit nil)
                       (offset nil)
                       (order-by nil)
                       (sort-order nil))
  ;; fred/tags - Get all tags, search for tags, or get tags by name.
  (let* ((rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-names-cons (and tag-names
                              (cons "tag_names" (conjoin-strings-for-query tag-names))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "tags"
                               (list-non-nil rt-start-cons
                                             rt-end-cons
                                             tag-names-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
     (values (parse-body-tags response parse-tag-tag)
             (xml-form-tag response))))

(defun fred-related-tags (&key tag-names
                               (realtime-start nil)
                               (realtime-end nil)
                               (tag-group-id nil)
                               (search-text nil)
                               (limit nil)
                               (offset nil)
                               (order-by nil)
                               (sort-order nil))
  ;; fred/related_tags - Get the related tags for one or more tags.
  (let* ((tag-names-cons (cons "tag_names" (conjoin-strings-for-query tag-names)))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (tag-group-id-cons (and tag-group-id
                                 (cons "tag_group_id" (tag-group-id-string tag-group-id))))
         (stext-cons (and search-text
                          (cons "search_text" (conjoin-search-strings search-text))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (tag-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "related_tags"
                               (list-non-nil tag-names-cons
                                             rt-start-cons
                                             rt-end-cons
                                             tag-group-id-cons
                                             stext-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-tag-tag)
             (xml-form-tag response))))

(defun fred-tags-series (&key tag-names
                              (realtime-start nil)
                              (realtime-end nil)
                              (limit nil)
                              (offset nil)
                              (order-by nil)
                              (sort-order nil))
  ;; fred/tags/series - Get the series matching tags.
  (let* ((tag-names-cons (cons "tag_names" (conjoin-strings-for-query tag-names)))
         (rt-start-cons (and realtime-start
                             (cons "realtime_start" (fred-date-string realtime-start))))
         (rt-end-cons (and realtime-end
                           (cons "realtime_end" (fred-date-string realtime-end))))
         (limit-cons (and limit
                          (cons "limit" (fred-int-string limit))))
         (offset-cons (and offset
                           (cons "offset" (fred-int-string offset))))
         (order-by-cons (and order-by
                             (cons "order_by" (series-order-by-string order-by))))
         (sorder-cons (and sort-order
                           (cons "sort_order" (sort-order-string sort-order))))
         (response (fred-query "tags/series"
                               (list-non-nil tag-names-cons
                                             rt-start-cons
                                             rt-end-cons
                                             limit-cons
                                             offset-cons
                                             order-by-cons
                                             sorder-cons))))
    (values (parse-body-tags response parse-series-tag)
             (xml-form-tag response))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search functions

(defun find-or-make-series (series-id)
  (or (find-series series-id)
      (nil-if-errors
       (destructuring-bind (id realtime-start realtime-end title observation-start 
                              observation-end frequency frequency-short units units-short
                              seasonal-adjustment seasonal-adjustment-short last-updated
                              popularity notes)
                           (fred-series :series-id series-id)
         (declare (ignore realtime-start realtime-end units-short seasonal-adjustment))
         (make-instance 'fred-data-series
           :id id
           :title title
           :start observation-start
           :end observation-end
           :freq frequency
           :short-freq frequency-short
           :units units
           :seas-adj seasonal-adjustment-short
           :last-update last-updated
           :popularity popularity
           :notes notes)))))

(defun search-for-series (search-term-list)
  (let ((found-series (fred-series-search :search-text search-term-list))
        (series-list nil))
    (dolist (series-info found-series)
      (destructuring-bind (id realtime-start realtime-end title observation-start 
                              observation-end frequency frequency-short units units-short
                              seasonal-adjustment seasonal-adjustment-short last-updated
                              popularity notes)
                          series-info
        (declare (ignore realtime-start realtime-end units-short seasonal-adjustment))
        (push (or (find-series id)
                  (make-instance 'fred-data-series
                    :id id
                    :title title
                    :start observation-start
                    :end observation-end
                    :freq frequency
                    :short-freq frequency-short
                    :units units
                    :seas-adj seasonal-adjustment-short
                    :last-update last-updated
                    :popularity popularity
                    :notes notes))
              series-list)))
    (nreverse series-list)))

(defun all-tag-groups ()
  (unless (plusp (hash-table-count *all-tag-groups*))
    (initialize-fred-tags))
  (let ((groups nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v groups))
             *all-tag-groups*)
    groups))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common data-series transform functions that can be used to create derived-data series

(defmethod derived-change-series ((series data-series))
  (make-instance 'derived-data-series
    :args (list series)
    :func #'change-transform
    :all t
    :units (concatenate 'string
                        "change of "
                        (series-units series))))

(defmethod change-transform ((series data-series))
  ;; computes the absolute change between series observations
  ;; change for first value will always be 0
  (with-slots (series-observations) series
    (when series-observations
      (let* ((ser-vals (coerce (series-observations series) 'list))
             (vals (cons (first ser-vals) ser-vals)))
        (butlast (maplist #'(lambda (vals)
                              (when (second vals)
                                (- (second vals) (first vals))))
                          vals))))))

(defmethod derived-percent-change-series ((series data-series))
  (make-instance 'derived-data-series
    :args (list series)
    :func #'percent-change-transform
    :all t
    :units (concatenate 'string
                        "change of "
                        (series-units series))))

(defmethod percent-change-transform ((series data-series))
  ;; computes the percentage change between series observations
  ;; change for first value will always be 0
  (with-slots (series-observations) series
    (when series-observations
      (let* ((ser-vals (coerce (series-observations series) 'list))
             (vals (cons (first ser-vals) ser-vals)))
        (butlast (maplist #'(lambda (vals)
                              (when (second vals)
                                (if (zerop (first vals))
                                    100.0 ;; not really correct obviously, but we can't represent infinity
                                    (* 100 (/ (- (second vals) (first vals))
                                              (first vals))))))
                          vals))))))

(defun percent-of-transform (val1 val2)
  (* 100.0 (/ val1 val2)))

(defmethod derived-%-gdp-series ((series data-series))
  (make-instance 'derived-data-series
    :args (list series)
    :func #'percent-of-gdp-transform
    :all t
    :units "percent of U.S. GDP"))

(defmethod percent-of-gdp-transform ((series data-series))
  ;; computes derived series as a percent of U.S. GDP
  ;; You could do the same thing by doing something like the following:
  ;; (make-instance 'derived-data-series
  ;;  :args (list <numerator-series> (find-series "GDP"))
  ;;  :func #'percent-of-transform
  ;;  :all nil)
  ;; which would compute the observation for any particular date each time it
  ;; is requested rather than computing them all at once, just one time as is done here.
  (let* ((gdp (find-or-make-series "GDP"))
         (ser-it (series-observation-iterator series))
         (mult (* 100 (/ (first (series-multipliers series))
                         (first (series-multipliers gdp)))))
        (vals nil))
    (do* ((dt-val-list (multiple-value-list (funcall ser-it))
                       (multiple-value-list (funcall ser-it)))
          (gdp-obs (nil-if-errors (series-observation gdp (first dt-val-list)))
                   (nil-if-errors (series-observation gdp (first dt-val-list)))))
         ((null (first dt-val-list)) (nreverse vals))
      (push (if (and gdp-obs (plusp gdp-obs))
                (* mult (/ (second dt-val-list) gdp-obs))
                0.0)
            vals))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Functions

#|

(in-package :fred)

(setf cat0 (make-instance 'fred-data-category :id "0"))
(inspect cat0)
(cat-children cat0)
(setf academic-data-cat (find-category "33060"))
(setf ad-series (first (cat-series academic-data-cat)))
(string-date (series-start-dt ad-series))
(string-date (series-end-dt ad-series))
(series-observations ad-series)
(setf raw-obs (mapcar #'(lambda (tuple)
                          (list (fred-date-string (first tuple))
                                (fred-date-string (second tuple))
                                (fred-date-string (third tuple))
                                (fourth tuple)))
                      (fred-series-observations :series-id "LODINIM066N")))
(series-observation ad-series (hist-date 2011 8 1))
(series-observation ad-series (hist-date 2011 8 22))
(series-observation ad-series (hist-date 2011 9 1))
(setf (series-interpolation-method ad-series) :prior)
;; rerun series-observation calls
(setf (series-interpolation-method ad-series) :closest)
;; rerun series-observation calls
(setf (series-interpolation-method ad-series) :avg)
;; rerun series-observation calls
(setf rel (series-release ad-series))
(search-for-series (list "weekly" "U.S."))
(mapcar #'(lamda (ser)
             (list (series-id ser) (series-title ser)))
        *)
(fred-series :series-id "BASE")
|#

(provide :fred)
