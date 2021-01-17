#pragma once
#ifndef PARSER_BASICS_H
#define PARSER_BASICS_H

#include "parse_error.h"

namespace tpcalc {

using widest_uint_type = std::uint64_t;
using widest_int_type = std::int64_t;

using float_type = double;

using parser_num_type = std::variant<
    std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
    std::uint32_t, std::int64_t, std::uint64_t, float_type>;
    // in order of ascending promotability

} // namespace tpcalc

#endif // PARSER_BASICS_H
