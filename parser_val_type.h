#pragma once
#ifndef PARSER_VAL_TYPE_H

#include "parser_num_type.h"
#include "parser_list_type.h"

namespace tpcalc {

using parser_val_type_base = std::variant<
    std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
    std::uint32_t, std::int64_t, std::uint64_t, float_type, list_type>;
    // elements correspond with parser_num_type (except for list_type).

static constexpr auto parser_val_type_short_txt = std::array
    // short text for UI.
    // elements correspond with val_type_base variant so index() can be used as index
    {"Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Flt. Pt.", "List"};

struct parser_val_type : parser_val_type_base {
// a variant type that can include lists of parser_num_types
    template <typename T> // for converting a parser_num_type alternative to a parser_val_type
    parser_val_type(const T& val) : parser_val_type_base{val} {}

    parser_val_type(const parser_num_type& num_val) { // convert parser_num_type to parser_val_type
        std::visit([&](const auto& val) {
            *this = val;
        }, num_val);
    }

    parser_val_type() = default;
};

template <typename T>
auto get_as(const parser_val_type& val_var) -> T {
// returns val_var with its value casted as T.
// precondition: any of parser_val_type's alternative types is convertable to T
// except list_type (case 9 below), for which T can only be list_type
    switch (val_var.index()) {
    case 0: return static_cast<T>(std::get<std::variant_alternative_t<0, parser_val_type_base>>(val_var));
    case 1: return static_cast<T>(std::get<std::variant_alternative_t<1, parser_val_type_base>>(val_var));
    case 2: return static_cast<T>(std::get<std::variant_alternative_t<2, parser_val_type_base>>(val_var));
    case 3: return static_cast<T>(std::get<std::variant_alternative_t<3, parser_val_type_base>>(val_var));
    case 4: return static_cast<T>(std::get<std::variant_alternative_t<4, parser_val_type_base>>(val_var));
    case 5: return static_cast<T>(std::get<std::variant_alternative_t<5, parser_val_type_base>>(val_var));
    case 6: return static_cast<T>(std::get<std::variant_alternative_t<6, parser_val_type_base>>(val_var));
    case 7: return static_cast<T>(std::get<std::variant_alternative_t<7, parser_val_type_base>>(val_var));
    case 8: return static_cast<T>(std::get<std::variant_alternative_t<8, parser_val_type_base>>(val_var));
    case 9:
        static_assert(std::is_same_v<list_type, std::variant_alternative_t<9, parser_val_type_base>>); // alternative 9 should be list_type
        if constexpr (std::is_same_v<T, list_type>)
            return std::get<T>(val_var);
        // else unsupported conversion to T
        [[fallthrough]];
    default: // missed one
        assert(false);
        throw internal_error{"get_as (3)"};
    }
}

inline auto get_as(size_t idx, const parser_val_type& val_var) -> parser_val_type {
// returns val_var with it's value casted as the alternative type indicated by
// idx (index).
// precondition: idx is a valid index
    if (idx == val_var.index())
        return val_var;
    parser_val_type val_var_;
    switch (idx) {
    case 0: val_var_ = get_as<std::int8_t>(val_var); break;
    case 1: val_var_ = get_as<std::uint8_t>(val_var); break;
    case 2: val_var_ = get_as<std::int16_t>(val_var); break;
    case 3: val_var_ = get_as<std::uint16_t>(val_var); break;
    case 4: val_var_ = get_as<std::int32_t>(val_var); break;
    case 5: val_var_ = get_as<std::uint32_t>(val_var); break;
    case 6: val_var_ = get_as<std::int64_t>(val_var); break;
    case 7: val_var_ = get_as<std::uint64_t>(val_var); break;
    case 8: val_var_ = get_as<float_type>(val_var); break;
    case 9: val_var_ = get_as<list_type>(val_var); break;
    default: // missed one or idx is invalid
        assert(false);
        throw internal_error{"parser<CharT>::get_as (4)"};
    }
    assert(idx == val_var_.index());
    return val_var_;
}

inline auto promoted(const parser_val_type& lval_var, const parser_val_type& rval_var) -> std::pair<parser_val_type, parser_val_type> {
// casts either lval_var's or rval_var's alternative to the higher promotion
// level of the two and returns the modified value along with the other as a
// pair
    if (lval_var.index() < rval_var.index())
        return {get_as(rval_var.index(), lval_var), rval_var};
    if (lval_var.index() > rval_var.index())
        return {lval_var, get_as(lval_var.index(), rval_var)};
    return {lval_var, rval_var};
}

template <typename Fn>
inline auto apply_promoted(const Fn& fn, const parser_val_type& lval_var, const parser_val_type& rval_var) -> auto {
// fn: functor/lambda with operator()(T, T) -> parser_val_type overloaded for
// each of parser_val_type's alternetive types (T). applies promoted() to
// lval_var and rval_var, then applies fn() to lval_var's and rval_var's
// promoted values. this function is used instead of std:visit to avoid
// generating exponential number of functions, which can be done because
// promoted() insures lval_var and rval_var will have the same alternative type
    auto [lval_var_, rval_var_] = promoted(lval_var, rval_var);
    assert(lval_var_.index() == rval_var_.index());
    switch (lval_var_.index()) {
    case 0: return fn(std::get<std::variant_alternative_t<0, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<0, parser_val_type_base>>(rval_var_));
    case 1: return fn(std::get<std::variant_alternative_t<1, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<1, parser_val_type_base>>(rval_var_));
    case 2: return fn(std::get<std::variant_alternative_t<2, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<2, parser_val_type_base>>(rval_var_));
    case 3: return fn(std::get<std::variant_alternative_t<3, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<3, parser_val_type_base>>(rval_var_));
    case 4: return fn(std::get<std::variant_alternative_t<4, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<4, parser_val_type_base>>(rval_var_));
    case 5: return fn(std::get<std::variant_alternative_t<5, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<5, parser_val_type_base>>(rval_var_));
    case 6: return fn(std::get<std::variant_alternative_t<6, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<6, parser_val_type_base>>(rval_var_));
    case 7: return fn(std::get<std::variant_alternative_t<7, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<7, parser_val_type_base>>(rval_var_));
    case 8: return fn(std::get<std::variant_alternative_t<8, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<8, parser_val_type_base>>(rval_var_));
    case 9: return fn(std::get<std::variant_alternative_t<9, parser_val_type_base>>(lval_var_), std::get<std::variant_alternative_t<9, parser_val_type_base>>(rval_var_));
    default: // missed one
        assert(false);
        throw internal_error{"parser<CharT>::apply_promoted (2)"};
    }
}

template <typename Op, typename CharT>
parser_val_type basic_or_vector_op(Op op, const parser_val_type& lval_var, const parser_val_type& rval_var, const error_context<CharT>& err_context) {
// support for an operation that may be:
// a basic operation (<scalar> <operator> <scalar>)
// or <vector> <operator> <scalar>
// or <scalar> <operator> <vector>
// or <scalar> <operator> <vector>
// or <vector> <operator> <vector>.
// op is a functor such as a lambda that performs a primitive operation such as <lval> + <rval>
    return std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
        return apply_op(op, lval, rval, err_context);
    }, lval_var, rval_var);
}

template <typename LT, typename RT, typename Op, typename CharT>
inline auto apply_op(Op op, const LT& lval, const RT& rval, const error_context<CharT>& err_context) {
// for basic operation (scalar op scalar); see basic_or_vector_op
    return op(lval, rval, err_context);
}

template <typename Scalar, typename Op, typename CharT>
list_type apply_op(Op op, const list_type& v, Scalar s, const error_context<CharT>& err_context) {
// for <vector> <operator> <scalar>; see basic_or_vector_op
    list_type v_;
    v_.reserve(v.size());
    for (auto& x : v)
        v_.emplace_back(std::visit([&](auto x_) -> parser_num_type {
            return op(x_, s, err_context);
        }, x));
    return v_;
}

template <typename Scalar, typename Op, typename CharT>
inline list_type apply_op(Op op, Scalar s, const list_type& v, const error_context<CharT>& err_context) {
// for <scalar> <operator> <vector>; see basic_or_vector_op
    return apply_op(op, v, s, err_context);
}

template <typename Op, typename CharT>
list_type apply_op(Op op, const list_type& lv, const list_type& rv, const error_context<CharT>& err_context) {
// for <vector> <operator> <vector>; see basic_or_vector_op
    if (lv.size() != rv.size())
        throw parse_error<CharT>(parse_error<CharT>::lists_must_be_same_size, err_context);
    auto lend = lv.begin() + lv.size();
	auto ritr = rv.begin();
    list_type v;
    v.reserve(lv.size());
	for (auto litr = lv.begin(); litr != lend; ++litr, ++ritr)
        v.emplace_back(
            apply_promoted([&](const auto& lval, const auto& rval) -> parser_num_type {
                return apply_op(op, lval, rval, err_context);
            }, *litr, *ritr)
        );
    return v;
}

} // namespace tpcalc

#endif // PARSER_VAL_TYPE_H