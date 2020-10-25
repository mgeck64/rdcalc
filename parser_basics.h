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

struct add_op {
    template <typename LT, typename RT, typename CharT>
    auto operator()(LT lval, RT rval, const error_context<CharT>&) -> auto {
        return lval + rval;
    }
};

struct sub_op {
    template <typename LT, typename RT, typename CharT>
    auto operator()(LT lval, RT rval, const error_context<CharT>&) -> auto {
        return lval - rval;
    }
};

struct mul_op {
    template <typename LT, typename RT, typename CharT>
    auto operator()(LT lval, RT rval, const error_context<CharT>&) -> auto {
        return lval * rval;
    }
};

struct div_op {
    template <typename LT, typename RT, typename CharT>
    auto operator()(LT lval, RT rval, const error_context<CharT>& err_context) -> auto {
        if constexpr (std::is_floating_point_v<LT> || std::is_floating_point_v<RT>)
            return lval / rval; // let divide by 0 result in inf
        else {
            if (rval == 0)
                throw parse_error<CharT>(parse_error<CharT>::division_by_0, err_context);
            return lval / rval;
        }
    }
};

struct exp_op {
    template <typename LT, typename RT, typename CharT>
    auto operator()(LT lval, RT rval, const error_context<CharT>&) -> auto {
        return pow(lval, rval);
    }
};

} // namespace tpcalc

#endif // PARSER_BASICS_H
