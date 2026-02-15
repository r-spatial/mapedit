# textshaping 1.0.4

* Guard against issues related to incompatible versions of freetype in
  systemfonts and textshaping (#68).

# textshaping 1.0.3

* Fixed a signed integer overflow in the the fix for ragg#193

# textshaping 1.0.2

* Fixed a bug in bidi embedding arrangement when shaping a single line
  (ggplot2#6497)
* Fixed a bug in shape caching due to a weak vector hash implementation
  (ragg#193)
* Fixed a bug in line positioning when line containes mix of different sizes

# textshaping 1.0.1

* Fixed a bug where hard line breaks where ignored if the line consisted of
  multiple embeddings (marquee#58)
* Fixed a bug where newline characters would increase the width of a line and
  lead to an empty line getting inserted in some situations (marquee#55)

# textshaping 1.0.0

* Added `lorem_text()` and `lorem_bidi()` for generating nonsense text in
  various scripts
* Added `plot_shape()` to plot the result of a shaping along with the metrics
* Rewrite of the shaping engine to honor global direction of text. It introduces
  a `direction` argument to `shape_text()` that defaults to `auto`, meaning that
  it is deduced from the content of the shaped text. `align` gets two new
  settings that responds to the global direction of the text. `"auto"` will
  chose between `"left"` and `"right"` and `"justified"` will choose between
  `"justified-left"` and `"justified-right"` depending of the global direction
  is ltr or rtl. Lastly the soft break locations are now based on ICU and thus
  better support ideographic scripts such as Han/Kanji/Hangul.
* Textshaping now properly supports soft hyphens in that a hyphen is rendered if
  a soft-wrap happens at a soft hyphen (#52)

# textshaping 0.4.1

* Make compiled code somewhat less assumptive about the correctness of the input
* Fix a bug from too aggressive early exiting shaping of strings with no max
  width (#45)
* Fixed a mismatch between the default values of the `width` argument in
  `shape_text()` and `systemfonts::match_fonts()` (#44)
* Updated `text_width()` to take the same inputs as `shape_text()`

# textshaping 0.4.0

* Full rewrite of `shape_text()` to allow proper font-fallback, bidi text
  support, support for font-features, spacers, new align settings, etc.

# textshaping 0.3.7

* Prepare for Arm Windows

# textshaping 0.3.6

* Fix a bug in fallback font loading which would crash the process if the font
  failed to load (#23)
* Fixed bug that would reset fallback to the original font for short strings
  (#25)

# textshaping 0.3.5

* Address an UBCSAN issue in packages linking to textshaping
* Remove a few compiler warnings

# textshaping 0.3.4

* Prepare textshaping for UCRT support
* Address upstream changes in cpp11

# textshaping 0.3.3

* Support static linking on macOS (#17, @jeroen)

# textshaping 0.3.2

* Avoid overindexing fallbacks when no fallback is found

# textshaping 0.3.1

* Try to avoid ASAN issue reported by CRAN

# textshaping 0.3.0

* Add support for performing font fallback as part of the single-line shaping
* Provide support for non-scalable fonts

# textshaping 0.2.1

* Fix issues with the Solaris mock solution

# textshaping 0.2.0

* Update C API to prepare for font fallback
* Make sure it compiles on Solaris without system dependencies

# textshaping 0.1.2

* Fix a bug in the interaction with the systemfonts font cache that could cause
  random crashes on some mac installations.

# textshaping 0.1.1

* Small changes to comply with next cpp11 version

# textshaping 0.1.0

* First release. Provide access to HarfBuzz shaping and FriBidi bidirectional
  script support.
