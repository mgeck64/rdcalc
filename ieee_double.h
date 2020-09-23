#pragma once
#ifndef IEEE_DOUBLE_H
#define IEEE_DOUBLE_H

#include <cstdint>
#include <limits>

class ieee_double {
// breaks up IEEE 794 double precision number (presumed to be how double is
// implemented here) into component parts in a manner that facilitates
// outputting one
private:
    static_assert(std::numeric_limits<double>::is_iec559); // IEEE 794
    bool is_negative_ = false;
    std::int16_t exponent_ = 0;
    std::uint64_t significand_ = 0;
    
public:
    explicit ieee_double(double d);
    ieee_double() = default;
    ieee_double(const ieee_double&) = default;

    auto is_negative() const {return is_negative_;}    
    auto exponent() const {return exponent_;}
    auto significand() const {return significand_;} // note: effectively shifted left 12 bits (zero'd exponent field)
    auto is_inf() const {return exponent_ == 1024 && significand_ == 0;}
    auto is_nan() const {return exponent_ == 1024 && significand_ != 0;}
    auto is_subnormal() const {return exponent_ == -1023 && significand_ != 0;}
    auto is_actual_zero() const {return exponent_ == -1023 && significand_ == 0;}
};

inline ieee_double::ieee_double(double d) {
    auto id = *reinterpret_cast<const std::uint64_t*>(&d);
    is_negative_ = id & 0x8000000000000000;
    exponent_ = ((id & 0x7ff0000000000000) >> 52) - 1023;
    significand_ = id & 0x000fffffffffffff;
};

#endif