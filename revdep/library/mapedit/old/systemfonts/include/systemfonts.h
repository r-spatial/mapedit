#pragma once

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <stdlib.h>
#include <stdint.h>
#include <limits.h>

#ifdef __cplusplus
#include <string>
#include <cstring>
#else
#include <string.h>
#endif

struct FontFeature {
  char feature[4];
  int setting;
};
typedef struct FontFeature FontFeature;
// A structure to pass around a single font with features (used by the C interface)
// A structure to pass around a single font with features (used by the C interface)
struct FontSettings {
  char file[PATH_MAX + 1];
  unsigned int index;
  const FontFeature* features;
  int n_features;

  FontSettings() : index(0), features(nullptr), n_features(0) {
    file[0] = '\0';
  }
  FontSettings(const char* p, unsigned int i, const FontFeature* f, int n) : index(i), features(f), n_features(n) {
    strncpy(file, p, PATH_MAX + 1);
    file[PATH_MAX] = '\0';
  }
};
typedef struct FontSettings FontSettings;

// A structure to pass around a single font with features and variable axes (used by the C interface)
struct FontSettings2 : public FontSettings {
  const int* axes;
  const int* coords;
  int n_axes;

  FontSettings2() : axes(nullptr), coords(nullptr), n_axes(0) {

  }
  FontSettings2(FontSettings x) : axes(nullptr), coords(nullptr), n_axes(0) {
    strncpy(file, x.file, PATH_MAX + 1);
    index = x.index;
    features = x.features;
    n_features = x.n_features;
  }
};
typedef struct FontSettings2 FontSettings2;

// Get the file and index of a font given by its name, along with italic and
// bold status. Writes filepath to `path` and returns the index
static inline int locate_font(const char *family, int italic, int bold, char *path, int max_path_length) {
  static int (*p_locate_font)(const char*, int, int, char*, int) = NULL;
  if (p_locate_font == NULL) {
    p_locate_font = (int (*)(const char *, int, int, char *, int)) R_GetCCallable("systemfonts", "locate_font");
  }
  return p_locate_font(family, italic, bold, path, max_path_length);
}
// Get the file and index of a font along with possible registered OpenType
// features, returned as a FontSettings object.
static inline FontSettings locate_font_with_features(const char *family, int italic, int bold) {
  static FontSettings (*p_locate_font_with_features)(const char*, int, int) = NULL;
  if (p_locate_font_with_features == NULL) {
    p_locate_font_with_features = (FontSettings (*)(const char *, int, int)) R_GetCCallable("systemfonts", "locate_font_with_features");
  }
  return p_locate_font_with_features(family, italic, bold);
}
// Get ascent, descent, and width of a glyph, given by its unicode number,
// fontfile and index, along with its size and the resolution. Returns 0 if
// successful
static inline int glyph_metrics(uint32_t code, const char* fontfile, int index,
                         double size, double res, double* ascent,
                         double* descent, double* width) {
  static int (*p_glyph_metrics)(uint32_t, const char*, int, double, double, double*, double*, double*) = NULL;
  if (p_glyph_metrics == NULL) {
    p_glyph_metrics = (int (*)(uint32_t, const char*, int, double, double, double*, double*, double*)) R_GetCCallable("systemfonts", "glyph_metrics");
  }
  return p_glyph_metrics(code, fontfile, index, size, res, ascent, descent, width);
}
// Calculate the width of a string based on a fontfile, index, size, and
// resolution. Writes it to width, and returns 0 if successful
static inline int string_width(const char* string, const char* fontfile, int index,
                        double size, double res, int include_bearing,
                        double* width) {
  static int (*p_string_width)(const char*, const char*, int, double, double, int, double*) = NULL;
  if (p_string_width == NULL) {
    p_string_width = (int (*)(const char*, const char*, int, double, double, int, double*)) R_GetCCallable("systemfonts", "string_width");
  }
  return p_string_width(string, fontfile, index, size, res, include_bearing, width);
}
// Calculate glyph positions for a string based on a fontfile, index, size, and
// resolution, and writes it to the x and y arrays. Returns 0 if successful.
static inline int string_shape(const char* string, const char* fontfile, int index,
                        double size, double res, double* x, double* y,
                        unsigned int max_length) {
  static int (*p_string_shape)(const char*, const char*, int, double, double, double*, double*, unsigned int) = NULL;
  if (p_string_shape == NULL) {
    p_string_shape = (int (*)(const char*, const char*, int, double, double, double*, double*, unsigned int)) R_GetCCallable("systemfonts", "string_shape");
  }
  return p_string_shape(string, fontfile, index, size, res, x, y, max_length);
}
// Get the file and index of a fallback font for the given string based on the
// given font and index
static inline FontSettings get_fallback(const char *string, const char *path, int index) {
  static FontSettings (*p_get_fallback)(const char*, const char*, int) = NULL;
  if (p_get_fallback == NULL) {
    p_get_fallback = (FontSettings (*)(const char*, const char*, int)) R_GetCCallable("systemfonts", "get_fallback");
  }
  return p_get_fallback(string, path, index);
}
// Get the weight of the font as encoded in the OTT/2 table
static inline int get_font_weight(const char *path, int index) {
  static int (*p_get_weight)(const char*, int) = NULL;
  if (p_get_weight == NULL) {
    p_get_weight = (int (*)(const char*, int)) R_GetCCallable("systemfonts", "font_weight");
  }
  return p_get_weight(path, index);
}
// Get the family name of the font as encoded in the font file. The name is
// written to the family argument, not exceeding `max_length`
static inline int get_font_family(const char *path, int index, char* family, int max_length) {
  static int (*p_get_family)(const char*, int, char*, int) = NULL;
  if (p_get_family == NULL) {
    p_get_family = (int (*)(const char*, int, char*, int)) R_GetCCallable("systemfonts", "font_family");
  }
  return p_get_family(path, index, family, max_length);
}
// Get the location of emojis written to the embedding array. A 0 indicate that
// the codepoint is not to be treated as emoji, a 1 indicate that it should,
static inline void detect_emoji_embedding(const uint32_t* string, int n, int* embedding, const char *path, int index) {
  static void (*p_detect_emoji_embedding)(const uint32_t*, int, int*, const char*, int) = NULL;
  if (p_detect_emoji_embedding == NULL) {
    p_detect_emoji_embedding = (void (*)(const uint32_t*, int, int*, const char*, int)) R_GetCCallable("systemfonts", "detect_emoji_embedding");
  }
  p_detect_emoji_embedding(string, n, embedding, path, index);
}

#ifdef __cplusplus
// Get the outline of a glyph as a <path> string
static inline std::string get_glyph_path(int glyph, double* t, const char* path, int index, double size, bool* no_outline) {
  static std::string (*p_get_glyph_path)(int, double*, const char*, int, double, bool*) = NULL;
  if (p_get_glyph_path == NULL) {
    p_get_glyph_path = (std::string (*)(int, double*, const char*, int, double, bool*)) R_GetCCallable("systemfonts", "get_glyph_path");
  }
  return p_get_glyph_path(glyph, t, path, index, size, no_outline);
}
#endif

// Get a raster of a glyph as a nativeRaster
static inline SEXP get_glyph_raster(int glyph, const char* path, int index, double size, double res, int color) {
  static SEXP (*p_get_glyph_raster)(int, const char*, int, double, double, int) = NULL;
  if (p_get_glyph_raster == NULL) {
    p_get_glyph_raster = (SEXP (*)(int, const char*, int, double, double, int)) R_GetCCallable("systemfonts", "get_glyph_raster");
  }
  return p_get_glyph_raster(glyph, path, index, size, res, color);
}

namespace systemfonts {
  namespace ver2 {
    // This API version uses FontSettings2 to pass around information of a font
    // with all it's settings etc.
    //
    // Support for font features and variable axes
    //
    // string_width and string_shape has been deprecated as textshaping provides
    // a better solution to this

    // Get the file and index of a font along with possible registered OpenType
    // features, returned as a FontSettings2 object. Support variable fonts
    static inline FontSettings2 locate_font(const char *family, double italic, double weight, double width, const int* axes, const int* coords, int n_axes) {
      static FontSettings2 (*p_locate_font_with_features)(const char*, double, double, double, const int*, const int*, int) = NULL;
      if (p_locate_font_with_features == NULL) {
        p_locate_font_with_features = (FontSettings2 (*)(const char*, double, double, double, const int*, const int*, int)) R_GetCCallable("systemfonts", "locate_font_with_features2");
      }
      return p_locate_font_with_features(family, italic, weight, width, axes, coords, n_axes);
    }

    // Get the file and index of a fallback font for the given string based on the
    // given font and index. Supports variable fonts
    static inline FontSettings2 get_fallback(const char *string, const FontSettings2& font) {
      static FontSettings2 (*p_get_fallback)(const char*, const FontSettings2&) = NULL;
      if (p_get_fallback == NULL) {
        p_get_fallback = (FontSettings2 (*)(const char*, const FontSettings2&)) R_GetCCallable("systemfonts", "get_fallback2");
      }
      return p_get_fallback(string, font);
    }
    // Get ascent, descent, and width of a glyph, given by its unicode number,
    // fontfile and index, along with its size and the resolution. Returns 0 if
    // successful
    static inline int glyph_metrics(uint32_t code, const FontSettings2& font, double size, double res, double* ascent, double* descent, double* width) {
      static int (*p_glyph_metrics)(uint32_t, const FontSettings2&, double, double, double*, double*, double*) = NULL;
      if (p_glyph_metrics == NULL) {
        p_glyph_metrics = (int (*)(uint32_t, const FontSettings2&, double, double, double*, double*, double*)) R_GetCCallable("systemfonts", "glyph_metrics2");
      }
      return p_glyph_metrics(code, font, size, res, ascent, descent, width);
    }
    // Get the weight of the font as encoded in the OTT/2 table
    static inline int get_font_weight(const FontSettings2& font) {
      static int (*p_get_weight)(const FontSettings2&) = NULL;
      if (p_get_weight == NULL) {
        p_get_weight = (int (*)(const FontSettings2&)) R_GetCCallable("systemfonts", "font_weight2");
      }
      return p_get_weight(font);
    }
    // Get the family name of the font as encoded in the font file. The name is
    // written to the family argument, not exceeding `max_length`
    static inline int get_font_family(const FontSettings2& font, char* family, int max_length) {
      static int (*p_get_family)(const char*, int, char*, int) = NULL;
      if (p_get_family == NULL) {
        p_get_family = (int (*)(const char*, int, char*, int)) R_GetCCallable("systemfonts", "font_family");
      }
      return p_get_family(font.file, font.index, family, max_length);
    }
    // Get the location of emojis written to the embedding array. A 0 indicate that
    // the codepoint is not to be treated as emoji, a 1 indicate that it should,
    static inline void detect_emoji_embedding(const uint32_t* string, int n, int* embedding, const FontSettings2& font) {
      static void (*p_detect_emoji_embedding)(const uint32_t*, int, int*, const char*, int) = NULL;
      if (p_detect_emoji_embedding == NULL) {
        p_detect_emoji_embedding = (void (*)(const uint32_t*, int, int*, const char*, int)) R_GetCCallable("systemfonts", "detect_emoji_embedding");
      }
      p_detect_emoji_embedding(string, n, embedding, font.file, font.index);
    }
#ifdef __cplusplus
    // Get the outline of a glyph as a <path> string
    static inline std::string get_glyph_path(int glyph, double* t, const FontSettings2& font, double size, bool* no_outline) {
      static std::string (*p_get_glyph_path)(int, double*, const FontSettings2&, double, bool*) = NULL;
      if (p_get_glyph_path == NULL) {
        p_get_glyph_path = (std::string (*)(int, double*, const FontSettings2&, double, bool*)) R_GetCCallable("systemfonts", "get_glyph_path2");
      }
      return p_get_glyph_path(glyph, t, font, size, no_outline);
    }
#endif
    // Get a raster of a glyph as a nativeRaster
    static inline SEXP get_glyph_raster(int glyph, const FontSettings2& font, double size, double res, int color) {
      static SEXP (*p_get_glyph_raster)(int, const FontSettings2&, double, double, int) = NULL;
      if (p_get_glyph_raster == NULL) {
        p_get_glyph_raster = (SEXP (*)(int, const FontSettings2&, double, double, int)) R_GetCCallable("systemfonts", "get_glyph_raster2");
      }
      return p_get_glyph_raster(glyph, font, size, res, color);
    }
  }
}
