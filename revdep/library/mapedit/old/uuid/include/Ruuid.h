#ifndef R_UUID_H__
#define R_UUID_H__

#include "uuid.h"

/* types to use with generate() */
#define GT_DEFAULT 0
#define GT_RANDOM  1
#define GT_TIME    2

/* exported functions: */
typedef int (*RU_generate_t)(uuid_t uuid, int type);
typedef int (*RU_parse_t)(const char *in_start, const char *in_end, uuid_t uu); /* in_end can be NULL */
typedef void(*RU_unparse_t)(uuid_t uuid, char *out, int lower);

/* sample use:

RU_generate_t generate_fn = (RU_generate_t) R_GetCCallable("uuid", "generate");
RU_parse_t    parse_fn    = (RU_parse_t) R_GetCCallable("uuid", "parse");
RU_unparse_t  unparse_fn  = (RU_unparse_t) R_GetCCallable("uuid", "unparse");

*/

#endif
