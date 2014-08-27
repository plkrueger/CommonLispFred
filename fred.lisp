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

(defvar *all-categories* (make-hash-table :test #'equal))

(defvar *all-series* (make-hash-table :test #'equal))

(defvar *all-releases* (make-hash-table :test #'equal))

(defvar *all-sources* (make-hash-table :test #'equal))

(defvar *all-tag-groups* (make-hash-table :test #'equal))

(defvar *all-tags* (make-hash-table :test #'equal))

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
     (truncate (date-diff dt start-dt :year)))
    (:quarterly
     (truncate (date-diff dt start-dt :quarter)))
    (:monthly
     (truncate (date-diff dt start-dt :month)))
    (:weekly
     (truncate (date-diff dt start-dt :week)))))

(defun period-dates (dt freq-key)
  ;; all period dates encoded as 00:00 AM on the first date of the period
  ;; Find the date in the series that immediately preceded the specified date, which
  ;; represents the current period relative to the dt argument.
  ;; Return dates for the prior period, current period, and next period.
  (multiple-value-bind (yr mm dd)
                       (hist-date-yr-month-day dt)
    (case freq-key
      (:annual
       (let ((current-dt (hist-date yr 01 01)))
         (values (inc-date current-dt -1 :year)
                 current-dt
                 (inc-date current-dt 1 :year))))
      (:quarterly
       (let* ((quarter-indx (floor (1- mm) 3))
              (current-dt (hist-date yr (1+ (* 3 quarter-indx)) 01)))
         (values (inc-date current-dt -1 :quarter)
                 current-dt
                 (inc-date current-dt 1 :quarter))))
      (:monthly
       (let ((current-dt (hist-date yr mm 01)))
         (values (inc-date current-dt -1 :month)
                 current-dt
                 (inc-date current-dt 1 :month))))
      (:weekly ;; means week ending on Saturday by default
       (let* ((prior-days (days-from :sat (day-of-wk dt)))
              (current-dt (inc-days (hist-date yr mm dd) (- prior-days))))
         (values (inc-date current-dt -1 :week)
                 current-dt
                 (inc-date current-dt 1 :week)))))))

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
                       (period-dates obs-dt freq)
    (let ((current-indx (data-index obs-dt start-dt freq)))
      (values prior-period-dt
              (1- current-indx)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRED initialization 

;; either call this function with your api-key or just modify the defvar for *fred-api-key*
;; or call this function with a file in your home directory that contains a quoted string
;; containing your api_key
(defun initialize-fred (&optional (api-key nil))
  (setf *fred-api-key* (or api-key
                           (and (probe-file "~/API_Key.txt")
                                (with-open-file (f "~/API_Key.txt")
                                  (string (read f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object location methods

(defun find-category (id)
  (gethash id *all-categories*))

(defun find-series (id)
  (gethash id *all-series*))

(defun find-release (id)
  (gethash id *all-releases*))

(defun find-source (id)
  (gethash id *all-sources*))

(defun find-tag (name)
  (gethash name *all-tags*))

(defun find-tag-group (id)
  (gethash id *all-tag-groups*))

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
  (setf (gethash (series-id self) *all-series*) self))

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
  ;; all FRED® tags and tag groups. There are approximately 5,000 tags, so it will take several minutes
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
      (declare (ignore realtime-start realtime-end frequency-short units_short seasonal-adjustment-short))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
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
                         series-popularity series-notes) self
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
        (setf series-units units)
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
  (let* ((obs-info (fred-series-observations :series-id (series-id self)))
         (num-obs (list-length obs-info))
         (obs-values (mapcar #'fourth obs-info)))
    (setf (slot-value self 'series-observations) (make-array (list num-obs)
                                                             :adjustable t
                                                             :fill-pointer t
                                                             :initial-contents obs-values))
    (setf (series-max self) (apply #'max obs-values))
    (setf (series-min self) (apply #'min obs-values))
    (setf (series-sum self) (apply #'+ obs-values))
    (setf (series-avg self) (/ (series-sum self) num-obs)))
   (slot-value self 'series-observations))

(defmethod series-observation ((self data-series) obs-date)
  ;; Recall that period values are associated with the first date within the period.
  ;; In most cases what we want to return is value that represents the period of time
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
           (if (< (- obs-date current-period-dt) (- next-period-dt obs-date))
             prior-val
             current-val))
          (:avg ;; interpolate based on the fraction of the period until obs-date
           (+ prior-val (* (- current-val prior-val)
                           (/ (- obs-date current-period-dt) (- next-period-dt current-period-dt))))))))))

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
      (declare (ignore realtime-start realtime-end frequency-short units_short seasonal-adjustment))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
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
      (declare (ignore realtime-start realtime-end frequency-short units_short seasonal-adjustment))
      (let ((series (or (find-series id)
                        (make-instance 'fred-data-series
                          :id id
                          :title title
                          :start observation-start
                          :end observation-end
                          :freq frequency
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
The xml parser being used wil transform:

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
                  (http-request (concatenate 'string "http://api.stlouisfed.org/fred/" query-string)
                                :parameters (acons "api_key" *fred-api-key* key-value-alist))))
         (first-obj (xml-form-tag result))
         (error-info  (and (listp first-obj)
                           (eq (xml-form-tag first-obj) :|error|)
                           (fifth first-obj))))
    (when error-info
      (error error-info))
    (remove-lf-strs result)))

(defun fred-string-to-num (str)
  (read-from-string str nil 0))

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
     (intl-date-string dt))
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
        (fred-string-to-key (xml-assoc :|frequency| tag))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Functions

#|
(in-package :fred)

(setf cat0 (make-instance 'fred-data-category :id "0"))
(inspect cat0)
(cat-children cat0)
(setf academic-data-cat (find-category "33060"))
(setf ad-series (first (cat-series academic-data-cat)))
(date-string (series-start-dt ad-series))
(date-string (series-end-dt ad-series))
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

|#

(provide :fred)
