#if defined(_MSC_VER) && _MSC_VER >= 1900
#include <stdarg.h>
#include <stdio.h>
int msvc_snprintf(
    char*       const _Buffer,
    size_t      const _BufferCount,
    char const* const _Format, ...)
{
    int _Result;
    va_list _ArgList;
    __crt_va_start(_ArgList, _Format);
#pragma warning(suppress:28719)    // __WARNING_BANNED_API_USAGE
    _Result = _vsnprintf(_Buffer, _BufferCount, _Format, _ArgList);
    __crt_va_end(_ArgList);
    return _Result;
}
#endif
