/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to 1 if you have the <Annot.h> header file. */
#define HAVE_ANNOT_H 1

/* Define to 1 if error.h is useable. */
/* #undef HAVE_ERROR_H */

/* Define to 1 if you have the <err.h> header file. */
#define HAVE_ERR_H 1

/* Define to 1 if you have the `getline' function. */
#define HAVE_GETLINE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the <PDFDocEncoding.h> header file. */
#define HAVE_PDFDOCENCODING_H 1

/* Define to 1 to enable adding of markup annotations (requires poppler-glib
   >= 0.26). */
#define HAVE_POPPLER_ANNOT_MARKUP 1

/* Define to 1 to enable writing of annotations (requires poppler-glib >=
   0.19.4). */
#define HAVE_POPPLER_ANNOT_WRITE 1

/* Define to 1 to enable case sensitive searching (requires poppler-glib >=
   0.22). */
#define HAVE_POPPLER_FIND_OPTS 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#define HAVE_PTRDIFF_T 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcspn' function. */
#define HAVE_STRCSPN 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Name of package */
#define PACKAGE "epdfinfo"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "politza@fh-trier.de"

/* Define to the full name of this package. */
#define PACKAGE_NAME "epdfinfo"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "epdfinfo 1.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "epdfinfo"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.0"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.0"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef ssize_t */
