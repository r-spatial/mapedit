#ifndef SYSTEMFONTS_FT_H
#define SYSTEMFONTS_FT_H

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#include <ft2build.h>
#include FT_FREETYPE_H
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "systemfonts.h"

// Retrieve an FT_Face from the cache and assigns it to the face pointer. The
// retrieved face should be destroyed with FT_Done_Face once no longer needed.
// Returns 0 if successful.
static inline FT_Face get_cached_face(const char* fontfile, int index,
                                      double size, double res, int* error) {
  static FT_Face (*p_get_cached_face)(const char*, int, double, double, int*) = NULL;
  if (p_get_cached_face == NULL) {
    p_get_cached_face = (FT_Face (*)(const char*, int, double, double, int*)) R_GetCCallable("systemfonts", "get_cached_face");
  }
  return p_get_cached_face(fontfile, index, size, res, error);
}

namespace systemfonts {
  namespace ver2 {
    // Retrieve an FT_Face from the cache and assigns it to the face pointer. The
    // retrieved face should be destroyed with FT_Done_Face once no longer needed.
    // Returns 0 if successful.
    static inline FT_Face get_cached_face(const FontSettings2& font, double size, double res, int* error) {
      static FT_Face (*p_get_cached_face)(const FontSettings2&, double, double, int*) = NULL;
      if (p_get_cached_face == NULL) {
        p_get_cached_face = (FT_Face (*)(const FontSettings2&, double, double, int*)) R_GetCCallable("systemfonts", "get_cached_face2");
      }
      return p_get_cached_face(font, size, res, error);
    }
    // Check if this header is compiled with the same version of freetype as
    // systemfonts has been compiled with
    static inline bool check_ft_version() {
      static bool (*p_check_ft_version)(int, int, int) = NULL;
      if (p_check_ft_version == NULL) {
        p_check_ft_version = (bool (*)(int, int, int)) R_GetCCallable("systemfonts", "check_ft_version");
      }
      return p_check_ft_version(FREETYPE_MAJOR, FREETYPE_MINOR, FREETYPE_PATCH);
    }
  }
}

#endif
