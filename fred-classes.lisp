;; fred-classes.lisp

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

This defines the classes used to hold data from the FRED® facility provided by the St. Louis
Federal Reserve Bank. It provides a Lisp interface to the FRED® API. Users of this interface must
first obtain a user account with the Federal Reserve and then request their own API key for use in
making queries.

The idea behind this implementation is that developers might want to use data obtained both from the FRED®
facility and possibly other sources. So a series of base classes are defined and then FRED®-specific
classes are derived from them. Methods provided in fred.lisp are defined either on a base class or
the FRED®-specific class, depending on whether or not they rely on the FRED®-specific API. Users wishing
to implement or use data from other sources may define their own classes that derive from the
base classes and define methods that are specific to those other sources. Consult fred.lisp to see
what methods must be defined for new classes.

The implemenation loads data from the FRED® facility in a lazy manner, only when a user first attempts to access it.

The class derived-data-series can be used to define a series with observation values that are derived in some
manner from one or more other series. If the derivation is a simple mathematical function of values from other
series on the same date, then those values can be generated at the time requested just by applyin the
transform function to values from the other series on the same date. Sometimes a derived series is a function
of observations from one or more argument series on multiple dates. For those, it is necessary to compute values
for the whole series that can be accessed just as values for other series are accessed. An example of such a 
derived series might be one that represents the change in values of some other series as a percentage of yet 
another series.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :fred-package))

(in-package :fred)

(defclass data-category ()
  ((cat-id :accessor cat-id
           :initarg :id)
   (cat-name :accessor cat-name
             :initarg :name)
   (cat-parent :accessor cat-parent
               :initarg :parent)
   (cat-notes :accessor cat-notes
              :initarg :notes)
   (cat-children :accessor cat-children)
   (cat-tags :accessor cat-tags)
   (cat-related :accessor cat-related)
   (cat-series :accessor cat-series))
  (:default-initargs
    :id "0"
    :name nil
    :parent nil
    :notes nil))

(defclass fred-data-category (data-category)
  ())

(defclass data-series ()
  ((series-id :accessor series-id
              :initarg :id)
   (series-title :accessor series-title
                 :initarg :title)
   (series-start-dt :accessor series-start-dt
                    :initarg :start)
   (series-end-dt :accessor series-end-dt
                  :initarg :end)
   (series-frequency :accessor series-frequency
                     :initarg :freq)  ;; :annual :quarterly :monthly :weekly
   (series-units :accessor series-units
                 :initarg :units)
   (series-seasonally-adj :accessor series-seasonally-adj
                          :initarg :seas-adj)
   (series-last-update-dt :accessor series-last-update-dt
                          :initarg :last-update)
   (series-popularity :accessor series-popularity
                      :initarg :popularity)
   (series-notes :accessor series-notes
                 :initarg :notes)
   (series-interpolation-method :accessor series-interpolation-method ;; one of :prior :current :avg :closest
                                :initarg :interp)
   (series-categories :accessor series-categories
                      :initarg :categories)
   (series-release :accessor series-release
                   :initarg :release)
   (series-tags :accessor series-tags
                :initarg :tags)
   (series-transform :accessor series-transform
                     :initarg :transform)
   (series-observations :accessor series-observations
                        :initarg :observations)
   (series-max :accessor series-max
               :initform 0)
   (series-min :accessor series-min
               :initform 0)
   (series-avg :accessor series-avg
               :initform 0)
   (series-sum :accessor series-sum
               :initform 0))
  (:default-initargs
    :id nil
    :title nil
    :start 0
    :end 0
    :freq nil
    :units nil
    :seas-adj nil
    :last-update 0
    :popularity 0
    :notes nil
    :interp :current
    :transform :lin))

(defclass fred-data-series (data-series)
  ()
  (:default-initargs
    :id "0"
    :freq :annual
    :units "U.S. dollars"))

(defclass data-release ()
  ((release-id :accessor release-id
               :initarg :id)
   (release-name :accessor release-name
                 :initarg :name)
   (release-press-release :accessor release-press-release
                          :initarg :press-release)
   (release-link :accessor release-link
                 :initarg :link)
   (release-notes :accessor release-notes
                  :initarg :notes)
   (release-dates :accessor release-dates)
   (release-series :accessor release-series)
   (release-sources :accessor release-sources)
   (release-tags :accessor release-tags))
  (:default-initargs
    :id "0"
    :name nil
    :press-release nil
    :link nil
    :notes nil))

(defclass fred-data-release (data-release)
  ())

(defclass data-source ()
  ((source-id :accessor source-id
              :initarg :id)
   (source-name :accessor source-name
                :initarg :name)
   (source-link :accessor source-link
                :initarg :link)
   (source-notes :accessor source-notes
                 :initarg :notes)
   (source-releases :accessor source-releases))
  (:default-initargs
    :id "0"
    :name nil
    :link ""
    :notes nil))

(defclass fred-data-source (data-source)
  ())

(defclass data-tag ()
  ((tag-name :accessor tag-name
             :initarg :name)
   (tag-group-id :accessor tag-group-id
                 :initarg :group-id)
   (tag-notes :accessor tag-notes
              :initarg :notes)
   (tag-created :accessor tag-created
                :initarg :created)
   (tag-popularity :accessor tag-popularity
                   :initarg :popularity)
   (tag-series-count :accessor tag-series-count
                     :initarg :series-count)
   (tag-group :accessor tag-group)
   (tag-series :accessor tag-series))
  (:default-initargs
    :name ""
    :group-id nil
    :notes nil
    :created 0
    :popularity 0
    :series-count 0))

(defclass fred-data-tag (data-tag)
  ())

(defclass data-tag-group ()
  ((tgroup-id :accessor tgroup-id
              :initarg :id)
   (tgroup-tags :accessor tgroup-tags
                :initform nil))
  (:default-initargs
    :id "0"))

(defclass fred-data-tag-group (data-tag-group)
  ())

(defclass derived-data-series (data-series)
  ((dds-transform-args :accessor dds-transform-args
                       :initarg :args)
   (dds-transform-func :accessor dds-transform-func
                  :initarg :func)
   (dds-transform-all :accessor dds-transform-all
                      :initarg :all))
  (:default-initargs
    :func #'identity
    :all t
    :categories nil
    :release nil
    :tags nil))

(provide :fred-classes)
    