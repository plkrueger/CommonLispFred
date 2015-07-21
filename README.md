CommonLispFred
==============

Lisp Interface to Federal Reserve Economic Data (FRED®)

This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis. 
Information obtained using this API is subject to the "Legal Notices, Information and Disclaimers" described
at https://research.stlouisfed.org/legal.html. Also see the copyright notice within each source file for other terms of use of this software.

The files fred-package.lisp, fred-classes.lisp, and fred.lisp together implement a Common Lisp interface to the Federal Reserve Economic Data (FRED®) API which is provided by the Federal Reserve Bank of St. Louis. In addition, this release includes hist-date.lisp which provides software to represent and manipulate historical dates as described later.

Requirements

This has been tested using the latest development version of the Clozure Common Lisp distribution (CCL Version 1.10-dev-r16162-trunk  for DarwinX8664 as of this writing). Although I have attempted to make this compatible with other distributions, it is always possible that some problem might arise. If you find one, file a GIT ticket and I’ll attempt to resolve it.

This package is set up as a quicklisp project (courtesy of a contribution by Paul Nathan) and can be loaded via a quickload call: 

	(ql:quickload :fred)
You must install quicklisp on your system before loading this code. It (automatically) uses the s-xml and drakma open source packages that are also quicklisp projects.

Note: until this has been accepted as a quicklisp project you can download from the git repository and (assuming you have quicklisp installed) and do:

? (push #p”<“path to directory where fred files have been downloaded>” asdf:*central-registry*)

which on my system returns:

(#P"/Users/paul/Lisp Stuff/MyLispCode/Economic Simulation/CommonLispFred/" #P"/Users/paul/quicklisp/quicklisp/")

Then you can just use a quickload command to load the FRED code:

? (ql:quickload :fred)

which should return something like:

To load "fred":

Load 1 ASDF system:

fred

; Loading "fred"

[package currency]................................

[package hist-date]...............................

[package fred]....................

(:FRED)

? (in-package :fred)

etc.

Developers must acquire a FRED® API key and provide it in one of three ways:
	1.	Call initialize-fred with the key provided as a string argument, or
	2.	Put the key in a file named “API_Key.txt” in your home directory, or
	3.	Modify the fred.lisp source file to initialize the (defvar *fred-api-key* ) form defined there with your key. 

To get an API key you must first have a FRED® account (see http://api.stlouisfed.org/useraccount/regiser/step1) and then request an API key (see http://api.stlouisfed.org/api_key.html).

The main FRED® access page is http://api.stlouisfed.org/datatools.html and from there you can access the developer tools page for more information about this API.

This code distribution implements two distinct interface API’s: a direct counterpart of the FRED® API and an object-oriented interface that largely hides the FRED® API. It transparently performs queries as needed to instantiate information accessed from slots of a set of classes that have been defined to hold FRED® data.

Direct FRED® API

This code contains exact counterparts of the FRED® API which can be directly called.

Any of the following functions may be used to directly access the FRED® API:

Category Functions
   fred-category
	fred/category - Get a category.
   fred-category-children
	red/category/children - Get the child categories for a specified parent category.
   fred-category-related
	red/category/related - Get the related categories for a category.
   fred-category-series
	fred/category/series - Get the series in a category.
   fred-category-tags
	fred/category/tags - Get the tags for a category.
   fred-category-related-tags
	red/category/related_tags - Get the related tags for a category.

Releases Functions
   fred-releases
      fred/releases - Get all releases of economic data.
   fred-releases-dates
      red/releases/dates - Get release dates for all releases of economic data.
   fred-release
      fred/release - Get a release of economic data.
   fred-release-dates
      fred/release/dates - Get release dates for a release of economic data.
   fred-release-series
      fred/release/series - Get the series on a release of economic data.
   fred-release-sources
      fred/release/sources - Get the sources for a release of economic data.
   fred-release-tags
      red/release/tags - Get the tags for a release.
   fred-release-related-tags
      fred/release/related_tags - Get the related tags for a release.

Series Functions
   fred-series
      fred/series
      Get an economic data series.
   fred-series-categories
      fred/series/categories
      Get the categories for an economic data series.
   fred-series-observations
      fred/series/observations
      Get the observations or data values for an economic data series.
   fred-series-release
      fred/series/release
      Get the release for an economic data series.
   fred-series-search
      red/series/search
      Get economic data series that match keywords.
   fred-series-search-tags
      red/series/search/tags
      Get the tags for a series search.
   fred-series-search-related-tags
      fred/series/search/related_tags
      Get the related tags for a series search.
   fred-series-tags 
      fred/series/tags
      Get the tags for an economic data series.
   fred-series-updates
      fred/series/updates
      Get economic data series sorted by when observations were updated on the FRED®
      server.
   fred-series-vintagedates
      fred/series/vintagedates
      Get the dates in history when a series' data values were revised or new data values 
      were released.

Source Functions
   fred-sources
      red/sources
      Get all sources of economic data.
   fred-source
      fred/source
      Get a source of economic data.
   fred-sourece-releases
      red/source/releases
      Get the releases for a source.

Tag Functions
   fred-tags
      fred/tags
      Get all tags, search for tags, or get tags by name.
   fred-related-tags
      fred/related_tags
      Get the related tags for one or more tags.
   fred-tags-series
      fred/tags/series 
      Get the series matching tags.

These functions return either a list or a list of lists, depending on the nature of the function. The lowest level list that is returned contains the set of values returned by the FRED® query in a pre-defined order that can easily be determined from the code. See the various parse-… functions for details on what is returned or look at how
these low-level calls are used by the class-oriented interface that is documented below.

Generally speaking, values returned from queries are appropriate Common Lisp objects like strings and integers. Unfortunately, common lisp dates can only represent dates after Jan 1, 1900, and earlier dates are represented in some data series. So this release includes an alternate representation of dates that is included in the hist-date.lisp source file. This is similar in some respects to common lisp dates (for example both represent both a date and time and later dates are always numerically larger than earlier dates). Routines are provided to convert to and from hist-dates and common lisp dates (whenever that is legal). Although hist-dates also represent the time of day, they do not include any representation of the time zone which common lisp dates do. The hist-date.lisp source file also contains a variety of useful date utility functions that operate on hist-dates. These make it easy to do various sorts of date computations, create hist-dates, obtain the day of the week for a date, and display dates using several different date and time formats. See hist-date.lisp for more information about what is available.

Even though some types of queries return id’s that are numeric, all id’s are left in the form of strings. Some returned values are converted from strings to keywords to make use easier. In most cases, arguments to the fred- functions can be provided either as strings or as lisp values which are automatically converted to the appropriate form for the query.

Various sorts of errors are detected which result in calls to the error function (which throws an exception). Any error returned from a FRED® query will also result in such an exception.

Not all of the functionality provided by the direct low-level FRED® API is available via the object-oriented API. In particular, FRED® queries provide the ability to specify the format of the returned data. This software always requests XML results, which are first parsed into lisp list structures from which the data values that are actually returned from query calls are extracted. All other query parameters provided by the FRED® API have lisp keyword argument counterparts in the direct API.

Object-Oriented API

The second API allows users to create objects that correspond to the basic FRED® objects: Category, Series, Release, Source, Tag, and Tag Group. When an object is created by a make-instance call that provides (for example) an :id keyword, a FRED® query is automatically generated to retrieve additional data for that instance. Some object slots represent relationships between objects (for example the cat-series slot in the category class which contains a list of all series objects for that category). When such a slot is first accessed, an appropriate FRED® query is generated to retrieve all the related objects and the results of that query are used to create corresponding instances and to set the value of the slot, which is then returned. This is a form of lazy instantiation of slot values; queries are only generated to fill in slot values when the slot-value is requested for a currently unbound slot.

The classes defined anticipate that a user may wish to represent data from several different sources. So base classes are defined and then corresponding FRED® classes are derived from each of the base classes. The lazy slot evaluation is defined only for the FRED® classes so that other classes that initialize data in some other manner can also be derived from the base classes if desired.

Base classes are:
   data-category
   data-series
   data-release
   data-source
   data-tag
   data-tag-group

and the corresponding FRED® classes are:
   fred-data-category
   fred-data-series
   fred-data-release
   fred-data-source
   fred-data-tag
   fred-data-tag-group

See the fred-classes.lisp source file for more details.

Data observations could also have been instantiated as first class objects, but in a design choice, they are maintained within an array that is contained in the series-observations slot of data-series objects. This makes access and use of observation values somewhat faster and saves space as well. All series have a well-defined date frequency which makes it possible to compute the array index for any requested date. 

A series-observation method is provided for data-series objects which will return an observation for a particular date. If the exact date does not exist in the series, then the value of the series-interpolation-method slot will specify how a value should be constructed from other values in the series. This slot should have a keyword value that is one of  the following:
	:current, :prior, :closest, or :average. 
The :current keyword specifies that the value corresponding to the period within which the requested date exists should be returned. The :prior keyword specifies that the value of the period prior to the one within which the requested date exists should be returned. The :closest keyword specifies that if the argument date is closer to the end of the period than to the beginning, then return the value for that period, otherwise return the value of the prior period. The :avg keyword interpolates between the prior period value and the current period value based on the elapsed time from the beginning of the period to the requested date. By default the current period value is returned.

FRED® series values for any given period (e.g. year, quarter, month, etc.) are associated with the first date of that period. This is a convenient choice because end-of-period dates are much more difficult to work with. For convenience in date computations I have encoded all dates at midnight 00:00 AM (unless a time was included in the date data returned by a FRED® query).
