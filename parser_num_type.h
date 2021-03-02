#pragma once
#ifndef PARSER_NUM_TYPE
#define PARSER_NUM_TYPE

#include "parser_basics.h"

namespace tpcalc {

using parser_num_type = std::variant<
    std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
    std::uint32_t, std::int64_t, std::uint64_t, float_type>;
    // in order of ascending promotability
// basic variant type that can be included in lists

template <typename T>
auto get_as(const parser_num_type& num_var) -> T {
// returns num_var with its value casted as T
// precondition: any of parser_num_type's variant types is convertable to T
    switch (num_var.index()) {
    case 0: return static_cast<T>(std::get<std::variant_alternative_t<0, parser_num_type>>(num_var));
    case 1: return static_cast<T>(std::get<std::variant_alternative_t<1, parser_num_type>>(num_var));
    case 2: return static_cast<T>(std::get<std::variant_alternative_t<2, parser_num_type>>(num_var));
    case 3: return static_cast<T>(std::get<std::variant_alternative_t<3, parser_num_type>>(num_var));
    case 4: return static_cast<T>(std::get<std::variant_alternative_t<4, parser_num_type>>(num_var));
    case 5: return static_cast<T>(std::get<std::variant_alternative_t<5, parser_num_type>>(num_var));
    case 6: return static_cast<T>(std::get<std::variant_alternative_t<6, parser_num_type>>(num_var));
    case 7: return static_cast<T>(std::get<std::variant_alternative_t<7, parser_num_type>>(num_var));
    case 8: return static_cast<T>(std::get<std::variant_alternative_t<8, parser_num_type>>(num_var));
    default: // missed one
        assert(false);
        throw internal_error{"get_as (1)"};
    }
}

inline auto get_as(const size_t& idx, const parser_num_type& num_var) -> parser_num_type {
// returns num_var with it's value casted as the alternative type indicated by
// idx (index).
// precondition: idx is a valid index
    if (idx == num_var.index())
        return num_var;
    parser_num_type num_var_;
    switch (idx) {
    case 0: num_var_ = get_as<std::int8_t>(num_var); break;
    case 1: num_var_ = get_as<std::uint8_t>(num_var); break;
    case 2: num_var_ = get_as<std::int16_t>(num_var); break;
    case 3: num_var_ = get_as<std::uint16_t>(num_var); break;
    case 4: num_var_ = get_as<std::int32_t>(num_var); break;
    case 5: num_var_ = get_as<std::uint32_t>(num_var); break;
    case 6: num_var_ = get_as<std::int64_t>(num_var); break;
    case 7: num_var_ = get_as<std::uint64_t>(num_var); break;
    case 8: num_var_ = get_as<float_type>(num_var); break;
    default: // missed one or invalid index
        assert(false);
        throw internal_error{"get_as (2)"};
    }
    assert(idx == num_var_.index());
    return num_var_;
}

inline auto promoted(const parser_num_type& lnum_var, const parser_num_type& rnum_var) -> std::pair<parser_num_type, parser_num_type> {
// casts either lnum_var's or rnum_var's alternative to the higher promotion
// level of the two and returns the modified value along with the other as a
// pair
    if (lnum_var.index() < rnum_var.index())
        return {get_as(rnum_var.index(), lnum_var), rnum_var};
    if (lnum_var.index() > rnum_var.index())
        return {lnum_var, get_as(lnum_var.index(), rnum_var)};
    return {lnum_var, rnum_var};
}

template <typename Fn>
inline auto apply_promoted(const Fn& fn, const parser_num_type& lnum_var, const parser_num_type& rnum_var) -> auto {
// fn: functor/lambda with operator()(T, T) -> parser_num_type overloaded for
// each of parser_num_type's alternetive types (T). applies promoted() to
// lnum_var and rnum_var, then applies fn() to lnum_var's and rnum_var's
// promoted values. this function is used instead of std:visit to avoid
// generating exponential number of functions, which can be done because
// promoted() insures lnum_var and rnum_var will have the same alternative type
    auto [lnum_var_, rnum_var_] = promoted(lnum_var, rnum_var);
    assert(lnum_var_.index() == rnum_var_.index());
    switch (lnum_var_.index()) {
    case 0: return fn(std::get<std::variant_alternative_t<0, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<0, parser_num_type>>(rnum_var_));
    case 1: return fn(std::get<std::variant_alternative_t<1, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<1, parser_num_type>>(rnum_var_));
    case 2: return fn(std::get<std::variant_alternative_t<2, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<2, parser_num_type>>(rnum_var_));
    case 3: return fn(std::get<std::variant_alternative_t<3, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<3, parser_num_type>>(rnum_var_));
    case 4: return fn(std::get<std::variant_alternative_t<4, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<4, parser_num_type>>(rnum_var_));
    case 5: return fn(std::get<std::variant_alternative_t<5, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<5, parser_num_type>>(rnum_var_));
    case 6: return fn(std::get<std::variant_alternative_t<6, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<6, parser_num_type>>(rnum_var_));
    case 7: return fn(std::get<std::variant_alternative_t<7, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<7, parser_num_type>>(rnum_var_));
    case 8: return fn(std::get<std::variant_alternative_t<8, parser_num_type>>(lnum_var_), std::get<std::variant_alternative_t<8, parser_num_type>>(rnum_var_));
    default: // missed one
        assert(false);
        throw internal_error{"apply_promoted (1)"};
    }
}

} // namespace tpcalc

#endif // PARSER_NUM_TYPE