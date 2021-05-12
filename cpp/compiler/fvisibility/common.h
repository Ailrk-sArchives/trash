#pragma once
#if __GNUC__ >= 4
#define DLL_API __attribute__((visibility("default")))
#define DLL_LOCAL __attribute__((visibility("hidden")))
#else
#define DLL_API
#define DLL_LOCAL
#endif
