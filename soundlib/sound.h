#pragma once
//#define DEBUG
#define BASS

#include <stdio.h>
#include <bass.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#ifdef DEBUG
inline static int debugprintf(const char *format, ...) {
   va_list arg;
   int done;
   va_start (arg, format);
   done = vfprintf(stdout, format, arg);
   va_end (arg);
   return done;
}
#else
inline static int debugprintf(const char *format, ...) {
	return 0;
}
#endif
