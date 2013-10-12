#pragma once
//#define DEBUG
#define BASS

#include <stdio.h>
#include <bass.h>
#include "list.h"
#define DLLEXPORT __declspec(dllexport)


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
