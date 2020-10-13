#pragma once
#ifndef CALC_UTIL_H

#include <limits>
#include <type_traits>

namespace calc_util {

// almost_equal:

template <typename T>
typename std::enable_if_t<std::is_floating_point_v<T>, bool> almost_equal(T x, T y, unsigned ulp = 1)
// see https://en.cppreference.com/w/cpp/types/numeric_limits/epsilon
{
    // the machine epsilon has to be scaled to the magnitude of the values used
    // and multiplied by the desired precision in ULPs (units in the last place)
    return std::fabs(x-y) <= std::numeric_limits<T>::epsilon() * std::fabs(x+y) * ulp
        // unless the result is subnormal
        || std::fabs(x-y) < std::numeric_limits<T>::min();
}

// resetter, make_resetter:

template <typename T>
struct resetter {
    T& var;
    T value;
    resetter(T& var_, const T& value_)
        : var(var_), value(value_) {}
    ~resetter() {var = value;}
};

template <typename T>
resetter<T> make_resetter(T& var, const T& value) {
    return resetter<T>(var, value);
}

}

#endif // CALC_UTIL_H