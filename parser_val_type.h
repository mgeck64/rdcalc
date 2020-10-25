#pragma once
#ifndef PARSER_VAL_TYPE_H

#include "parser_list_type.h"

namespace tpcalc {

using parser_val_type_base = std::variant<
    std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
    std::uint32_t, std::int64_t, std::uint64_t, float_type, list_type>;
    // elements correspond with parser_num_type (except for list_type)

static constexpr auto parser_val_type_short_txt = std::array
    // short text for UI.
    // elements correspond with val_type_base variant so index() can be used as index
    {"Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Flt. Pt.", "List"};

struct parser_val_type : parser_val_type_base {
    template <typename T> // for converting parser_num_type alternative to parser_val_type
    parser_val_type(const T& val) : parser_val_type_base{val} {}

    parser_val_type(const parser_num_type& num_val) { // convert parser_num_type to parser_val_type
        std::visit([&](const auto& val) {
            *this = val;
        }, num_val);
    }

    parser_val_type() = default;
};

template <typename LT, typename RT, typename Op, typename CharT>
inline auto apply_op(Op op, const LT& lval, const RT& rval, const error_context<CharT>& err_context) {
    return op(lval, rval, err_context);
}

template <typename Scalar, typename Op, typename CharT>
list_type apply_op(Op op, const list_type& v, Scalar s, const error_context<CharT>& err_context) {
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
    return apply_op(op, v, s, err_context);
}

template <typename Op, typename CharT>
parser_num_type apply_op(Op op, const parser_num_type& lval_var, const parser_num_type& rval_var, const error_context<CharT>& err_context) {
    return std::visit([&](const auto& lval, const auto& rval) -> parser_num_type {
        return apply_op(op, lval, rval, err_context);
    }, lval_var, rval_var);
}

template <typename Op, typename CharT>
list_type apply_op(Op op, const list_type& lv, const list_type& rv, const error_context<CharT>& err_context) {
    auto min_size = std::min(lv.size(), rv.size());
    auto lend = lv.begin() + min_size;
	auto ritr = rv.begin();
    list_type v;
    v.reserve(min_size);
	for (auto litr = lv.begin(); litr != lend; ++litr, ++ritr)
        v.emplace_back(apply_op(op, *litr, *ritr, err_context));
    return v;
}

template <typename Op, typename CharT>
parser_val_type apply_op(Op op, const parser_val_type& lval_var, const parser_val_type& rval_var, const error_context<CharT>& err_context) {
    return std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
        return apply_op(op, lval, rval, err_context);
    }, lval_var, rval_var);
}

} // namespace tpcalc

#endif // PARSER_VAL_TYPE_H