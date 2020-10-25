#pragma once
#ifndef CHAR_HELPER_H
#define CHAR_HELPER_H

#include <cctype>
#include <cstdlib>
#include <cwchar>
#include <string>

namespace char_helper {

// for type char

inline bool isspace(char c) {return ::isspace(c);}
inline bool isdigit(char c) {return ::isdigit(c);}
inline bool isalpha(char c) {return ::isalpha(c);}
inline bool isalnum(char c) {return ::isalnum(c);}
inline bool isxdigit(char c) {return ::isxdigit(c);}
inline char tolower(char c) {return static_cast<char>(::tolower(c));}
inline double strtod(const char* str, const char*& endptr) {return ::strtod(str, const_cast<char**>(&endptr));}
inline long long strtoll(const char* str, const char*& endptr, unsigned base) {return ::strtoll(str, const_cast<char**>(&endptr), base);}
inline unsigned long long strtoull(const char* str, const char*& endptr, unsigned base) {return ::strtoull(str, const_cast<char**>(&endptr), base);}
inline size_t strlen(const char* str) {return ::strlen(str);}
inline void append_to(std::string& dst, std::string_view src) {dst += src;}
inline bool eq(std::string_view l, const char* r) {return l == r;}

// for type wchar_t

inline bool isspace(wchar_t c) {return ::iswspace(c);}
inline bool isdigit(wchar_t c) {return ::iswdigit(c);}
inline bool isalpha(wchar_t c) {return ::iswalpha(c);}
inline bool isalnum(wchar_t c) {return ::iswalnum(c);}
inline bool isxdigit(wchar_t c) {return ::iswxdigit(c);}
inline wchar_t tolower(wchar_t c) {return static_cast<wchar_t>(::tolower(c));}
inline unsigned long long strtoull(const wchar_t* str, const wchar_t*& endptr, unsigned base) {return ::wcstoull(str, const_cast<wchar_t**>(&endptr), base);}
inline double strtod(const wchar_t* str, const wchar_t*& endptr) {return ::wcstod(str, const_cast<wchar_t**>(&endptr));}
inline size_t strlen(const wchar_t* str) {return ::wcslen(str);}
inline void append_to(std::wstring& dst, std::wstring_view src) {dst += src;}
inline bool eq(std::wstring_view l, const wchar_t* r) {return l == r;}

inline void append_to(std::wstring& dst, const char* src) { // append narrow char. c-style string to wide char. string
    for (auto pos = src; *pos; ++pos) // assume string grows efficiently
        dst += *pos;
}

inline bool eq(std::wstring_view l, const char* r) { // compare wide char. string to narrow char. c-style string
    if (l.size() != strlen(r)) // in most cases l and r won't match
        return false;
    auto rpos = r;
    for (auto lpos = l.begin(); lpos != l.end(); ++lpos, ++rpos)
        if (*lpos != *rpos)
            return false;
    return true;
}

} // namespace char_helper

#endif // CHAR_HELPER_H
