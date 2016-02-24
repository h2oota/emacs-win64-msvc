#if defined(_MSC_VER) && _MSC_VER >= 19 && defined(_NO_CRT_STDIO_INLINE)
#undef _NO_CRT_STDIO_INLINE
#include <stdio.h>
const static void * stdio_functions[] = {
  printf, vprintf,  vfprintf, vsprintf,
  _snprintf, snprintf,
  sscanf,
};
#endif
