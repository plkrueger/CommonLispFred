;; fred-package.lisp

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

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :setup "home:quicklisp;setup.lisp")
  (ql:quickload :s-xml)
  (ql:quickload :drakma)
  (require :hist-date))

(defpackage :fred
  (:use :common-lisp :s-xml :drakma :hist-date)
  (:export
   ;; Class Names
   data-category
   fred-data-category
   data-series
   fred-data-series
   data-release
   fred-data-release
   data-source
   fred-data-source
   data-tag
   fred-data-tag
   data-tag-group
   fred-data-tag-group

   ;; data-category slot accessors
   cat-id
   cat-name
   cat-parent
   cat-notes
   cat-children
   cat-tags
   cat-related
   cat-series

   ;; data-series slot accessors
   series-id
   series-title
   series-start-dt
   series-end-dt
   series-frequency
   series-units
   series-seasonally-adj
   series-last-update-dt
   series-popularity
   series-notes
   series-interpolation-method
   series-categories
   series-release
   series-tags
   series-observations
   series-max
   series-min
   series-avg
   series-sum

   ;; data-release slot accessors
   release-id
   release-name
   release-press-release
   release-link
   release-notes
   release-dates
   release-series
   release-sources
   release-tags

   ;; data-source slot accessors
   source-id
   source-name
   source-link
   source-notes
   source-releases

   ;; data-tag slot accessors
   tag-name
   tag-group-id
   tag-notes
   tag-created
   tag-popularity
   tag-series-count
   tag-series

   ;; data-tag-group slot accessors
   tgroup-id
   tgroup-tags

   ;; utility function calls
   find-category
   find-release
   find-series
   find-source
   find-tag
   find-tag-group
   initialize-fred
   initialize-fred-categories

   ;; Below are direct fred API calls

   ;; Category
   fred-category                     ;; fred/category - Get a category.
   fred-category-children            ;; fred/category/children - Get the child categories for a specified parent category.
   fred-category-related             ;; fred/category/related - Get the related categories for a category.
   fred-category-series              ;; fred/category/series - Get the series in a category.
   fred-category-tags                ;; fred/category/tags - Get the tags for a category.
   fred-category-related-tags        ;; fred/category/related_tags - Get the related tags for a category.

   ;; Releases
   fred-releases                     ;; fred/releases - Get all releases of economic data.
   fred-releases-dates               ;; fred/releases/dates - Get release dates for all releases of economic data.
   fred-release                      ;; fred/release - Get a release of economic data.
   fred-release-dates                ;; fred/release/dates - Get release dates for a release of economic data.
   fred-release-series               ;; fred/release/series - Get the series on a release of economic data.
   fred-release-sources              ;; fred/release/sources - Get the sources for a release of economic data.
   fred-release-tags                 ;; fred/release/tags - Get the tags for a release.
   fred-release-related-tags         ;; fred/release/related_tags - Get the related tags for a release.

   ;; Series
   fred-series                       ;; fred/series - Get an economic data series.
   fred-series-categories            ;; fred/series/categories - Get the categories for an economic data series.
   fred-series-observations          ;; fred/series/observations - Get the observations or data values for an economic data series.
   fred-series-release               ;; fred/series/release - Get the release for an economic data series.
   fred-series-search                ;; fred/series/search - Get economic data series that match keywords.
   fred-series-search-tags           ;; fred/series/search/tags - Get the tags for a series search.
   fred-series-search-related-tags   ;; fred/series/search/related_tags - Get the related tags for a series search.
   fred-series-tags                  ;; fred/series/tags - Get the tags for an economic data series.
   fred-series-updates               ;; fred/series/updates - Get economic data series sorted by when observations were updated on the FRED® server.
   fred-series-vintagedates          ;; fred/series/vintagedates - Get the dates in history when a series' data values were revised or new data values were released.

   ;; Sources
   fred-sources                      ;; fred/sources - Get all sources of economic data.
   fred-source                       ;; fred/source - Get a source of economic data.
   fred-sourece-releases             ;; fred/source/releases - Get the releases for a source.

   ;; Tags
   fred-tags                         ;; fred/tags - Get all tags, search for tags, or get tags by name.
   fred-related-tags                 ;; fred/related_tags - Get the related tags for one or more tags.
   fred-tags-series                  ;; fred/tags/series - Get the series matching tags.

))

(provide :fred-package)