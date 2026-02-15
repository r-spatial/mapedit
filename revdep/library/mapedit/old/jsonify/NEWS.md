## v1.2.3

* removed C++11 System requirement
* patch for handling Int64 [issue 81](https://github.com/symbolixAU/jsonify/issues/81)

## v1.2.2

* skipped date tests on CRAN

## v1.2.1

* patch for correctly allocating strings [issue 61](https://github.com/symbolixAU/jsonify/issues/61)

## v1.2.0

* fixed C Stack Overflow caused by recursion [issue 61](https://github.com/symbolixAU/jsonify/issues/61)
* Simplified and templated more of the .hpp code

## v1.1.0

* removed Boost dependency
* set `stringsAsFactors = TRUE` in tests to comply with R 4.0.0
* implemented `from_ndjson()` and `to_ndjson()` [issue 29](https://github.com/symbolixAU/jsonify/issues/29) and [issue 58](https://github.com/symbolixAU/jsonify/issues/58)
* performance improvement for factors [issue 59](https://github.com/symbolixAU/jsonify/issues/59)
* Fixed encoding issues on Windows [issue 56](https://github.com/symbolixAU/jsonify/issues/56)

## v1.0

* empty list names handled [issue 53](https://github.com/SymbolixAU/jsonify/issues/53)
* `from_json()` method from converting JSON to R objects

## v0.2.1

* `as.json()` method
* Bug fix: AsIs data.frame inside another data.frame [issue #38](https://github.com/SymbolixAU/jsonify/issues/38)
* BH -DBOOST_NO_AUTO_PTR flag

## v0.2.0

* by column | row option
* factors_as_string argument
* Support for LANGSXP objects
* Dates are handled in c++, and now supports nested lists
* c++ API restructured and simplified
* `print.json()` method to print json class objects
* `minify_json()` function to remove indentation
* `pretty_json()` function to add indentation
* `digits` argument to specify the number of digits to which numeric values will be rounded

## v0.1.2

* `unbox` argument for `to_json()`
* Added a `NEWS.md` file to track changes to the package.
