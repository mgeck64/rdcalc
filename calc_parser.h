#pragma once
#ifndef CALC_PARSER_H
#define CALC_PARSER_H

#include "parser_val_type.h"
#include "lookahead.h"
#include <map>
#include <set>
#include <functional>
#include <algorithm>

namespace tpcalc {

template <typename CharT> // CharT: char or wchar_t
class parser {
public:
    using token = token<CharT>;
    using token_ids = typename token::token_ids;
    using token_error_codes = typename token::error_codes;
    using string_view = std::basic_string_view<CharT>;
    using string = std::basic_string<CharT>;
    using radices = typename token::radices;
    using error_context = error_context<CharT>;
    using parse_error = parse_error<CharT>;
    using error_codes = typename parse_error::error_codes;
    using lookahead_lexer = lookahead_lexer<CharT>;
    using notify_vars_changed_fn = std::function<void()>;

    parser(const notify_vars_changed_fn& notify_vars_changed_)
        : notify_vars_changed{notify_vars_changed_} {}

    parser() = default;

    parser(const parser&) = delete;
    parser& operator=(parser&) = delete;
    // disable copy/assignment; don't think can trivially copy vars_ below
    // because of how its elements bind to var_keys

    enum int_result_tags : unsigned {
        int8_tag, uint8_tag, int16_tag, uint16_tag, int32_tag, uint32_tag, int64_tag, uint64_tag};
        // must correspond with parser_val_type's alternative integer types
    auto int_result_tag() const -> auto {return int_result_tag_;}
    void int_result_tag(int_result_tags int_result_tag);

    auto default_radix() -> radices {return default_radix_;}
    auto default_radix(radices default_radix) -> void {default_radix_ = default_radix;}

    auto evaluate(const CharT* input) -> bool;
    // evaluates the input string; throws parse_error on parsing error. input is
    // as specified for lexer. uses a lexer object to which input is assigned
    // (see); the lexer object persists only for duration of this function call.
    // returns false of the input string was all empty or all whitespace; true
    // otherwise.

    auto last_val() const -> parser_val_type;
    // returns last evaluated result

    using var_keys_type = std::multiset<string>;
    struct variable {
        const typename var_keys_type::iterator key_pos; // see var_keys below
        parser_val_type val_var;
    };
    using vars_type = std::map<string_view, variable>;
    const vars_type& vars() const {return vars_;}

private:
    radices default_radix_ = radices::decimal;
    int_result_tags int_result_tag_ = int64_tag;

    parser_val_type last_val_ = 0.0;
    // result of last evaluation of non-empty input, accessable in an input
    // expression as "last"

    vars_type vars_;
    // key is string_view to allow direct and efficient comparison with
    // token::tok_str

    var_keys_type var_keys;
    // keys need to persist between evaluate() calls so copies will be stored
    // here; keys in vars_ will be views of strings stored here.
    // variable::key_pos points to corresponding entry here so it can be deleted
    // when entry in vars_ is deleted

    // a crucial reason vars_ and var_keys are map/multiset respectively is
    // because iterators need to remain valid after changes are made to the
    // containers; this eliminates vector despite the appeal of its simplicity

    template <typename T>
    static auto get_as(const parser_val_type& val_var) -> T;
    // returns val_var with its value casted as T; precondition: T is one of
    // parser_val_type's alternative types

    static auto get_as(const size_t& idx, const parser_val_type& val_var) -> parser_val_type;
    // returns val_var with it's value casted as the alternative type indicated
    // by idx (index)

    static auto promoted(const parser_val_type& lval_var, const parser_val_type& rval_var) -> std::pair<parser_val_type, parser_val_type>;
    // casts either lval_var's or rval_var's alternative to the higher promotion
    // level of the two and returns the modified value along with the other as a
    // pair

    template <typename Fn>
    static auto apply_promoted(const Fn& fn, const parser_val_type& lval_var, const parser_val_type& rval_var) -> auto;
    // fn: functor/lambda with operator()(T, T) -> parser_val_type overloaded
    // for each of parser_val_type's alternetive types (T). applies promoted()
    // to lval_var and rval_var, then applies fn() to lval_var's and rval_var's
    // promoted values. this function is used instead of std:visit to avoid
    // generating exponential number of functions, which can be done because
    // promoted() insures lval_var and rval_var will have the same alternative
    // type

    auto casted(const parser_val_type& val_var) const -> parser_val_type;
    // if val_var is an integer type then this casts it to the type indicated by
    // int_result_tag_; else (for float_type) this simply returns val_var

    // parser productions
    auto expression(lookahead_lexer& lexer) -> parser_val_type;
    auto arithmetic_expr(lookahead_lexer& lexer)-> parser_val_type;
    auto bor_term(lookahead_lexer& lexer) -> parser_val_type;
    auto bxor_term(lookahead_lexer& lexer) -> parser_val_type;
    auto band_term(lookahead_lexer& lexer) -> parser_val_type;
    auto shift_term(lookahead_lexer& lexer) -> parser_val_type;
    auto term(lookahead_lexer& lexer)-> parser_val_type;
    auto factor(lookahead_lexer& lexer)-> parser_val_type;
    auto primary_expression(lookahead_lexer& lexer)-> parser_val_type;
    auto identifier(lookahead_lexer& lexer)-> parser_val_type;
    auto group(lookahead_lexer& lexer) -> parser_val_type;
    auto list(lookahead_lexer& lexer) -> list_type;

    using unary_fn = float_type (*)(float_type);
    static std::array<std::pair<const char*, unary_fn>, 20> unary_fn_table;
    // unary_fn_table: simple unordered array; expected to be small enough that
    // simple linear search will be adequate

    using list_fn = parser_val_type (*)(const list_type&);
    static std::array<std::pair<const char*, list_fn>, 21> list_fn_table;
    // list_fn_table: simple unordered array; expected to be small enough that
    // simple linear search will be adequate

    static bool identifiers_match(string_view inp_symb, const char* tab_symb);
    // input identifier matches identifier in table?
    // internally, identifier in table is const char* regardless of CharT

    static auto sum(const list_type& list) -> parser_val_type;
    static auto prod(const list_type& list) -> parser_val_type;
    static auto avg(const list_type& list) -> parser_val_type;
    static auto geomean(const list_type& list) -> parser_val_type;
    static auto harmmean(const list_type& list) -> parser_val_type;
    static auto variance(const list_type& list, typename list_type::size_type n_adjustment) -> float_type;
    static auto variance(const list_type& list) -> parser_val_type;
    static auto stddev(const list_type& list) -> parser_val_type;
    static auto pvariance(const list_type& list) -> parser_val_type;
    static auto pstddev(const list_type& list) -> parser_val_type;
    static auto quantile(const list_type& list, float_type percent) -> float_type;
    static auto median(const list_type& list) -> parser_val_type;
    static auto mode(const list_type& list) -> parser_val_type;
    static auto min(const list_type& list) -> parser_val_type;
    static auto max(const list_type& list) -> parser_val_type;
    static auto quartile1(const list_type& list) -> parser_val_type;
    static auto quartile2(const list_type& list) -> parser_val_type;
    static auto quartile3(const list_type& list) -> parser_val_type;
    static auto iqr(const list_type& list) -> parser_val_type;
    static auto range(const list_type& list) -> parser_val_type;
    static auto madmean(const list_type& list) -> parser_val_type;
    static auto madmed(const list_type& list) -> parser_val_type;
    static auto qdev(const list_type& list) -> parser_val_type;

    static constexpr auto pi = 3.14159265358979323846;
    static constexpr auto e = 2.71828182845904523536;

    notify_vars_changed_fn notify_vars_changed = []{}; // initialize to do-nothing lambda
};

template <typename CharT>
inline auto parser<CharT>::last_val() const -> parser_val_type {
    return last_val_;
}

// TODO: am casting between int types with, in many cases, undefined or
// implementation defined behavior; should replace that with own well-defined
// casts but don't have time to do that right now so will live with MSVC++'s
// implemented behavior

template <typename CharT>
void parser<CharT>::int_result_tag(int_result_tags int_result_tag) {
    int_result_tag_ = int_result_tag;
    last_val_ = casted(last_val_);
    // cast last_val_ so that it shows that way on UI but also so the following
    // will work: if last_val_ was int16_t(255) and then was cast to int8_t,
    // becoming int8_t(-1), and then was cast back to int16_t then it will end
    // up being -1 (sign extended), not 255

// i changed my mind about applying this to the variables; doesn't seem right to
// change their format especially for a narrowing conversion.
//    // do the same for all variables
//    for (auto& var : vars_)
//        var.val_var = casted(var.val_var);
}

template <typename CharT>
template <typename T>
auto parser<CharT>::get_as(const parser_val_type& val_var) -> T {
    // precondition: any of parser_val_type's types is convertable to T (except for
    // list_type, case 9 below)
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
        // else unsupported conversion to T; calling code should preclude this.
        // fallthru to default
    default: // missed one
        assert(false);
        throw internal_error{"parser<CharT>::get_as (1)"};
    }
}

template <typename CharT>
auto parser<CharT>::get_as(const size_t& idx, const parser_val_type& val_var) -> parser_val_type {
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
    default: // missed one
        assert(false);
        throw internal_error{"parser<CharT>::get_as (2)"};
    }
    assert(idx == val_var_.index());
    return val_var_;
}

template <typename CharT>
auto parser<CharT>::promoted(const parser_val_type& lval_var, const parser_val_type& rval_var) -> std::pair<parser_val_type, parser_val_type> {
    if (lval_var.index() < rval_var.index())
        return {get_as(rval_var.index(), lval_var), rval_var};
    if (lval_var.index() > rval_var.index())
        return {lval_var, get_as(lval_var.index(), rval_var)};
    return {lval_var, rval_var};
}

template <typename CharT>
template <typename Fn>
inline auto parser<CharT>::apply_promoted(const Fn& fn, const parser_val_type& lval_var, const parser_val_type& rval_var) -> auto {
    auto [lval_var_, rval_var_] = std::move(promoted(lval_var, rval_var));
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
        throw internal_error{"parser<CharT>::apply_promoted"};
    }
}

template <typename CharT>
auto parser<CharT>::casted(const parser_val_type& val_var) const -> parser_val_type {
    return std::visit([&](const auto& val) -> parser_val_type {
        using VT = std::decay_t<decltype(val)>;
        if constexpr (std::is_integral_v<VT>)
            return get_as(int_result_tag_, val_var);
        else if constexpr (std::is_floating_point_v<VT> || std::is_same_v<VT, list_type>)
            return val;
            // TODO: for list_type, not sure if casted should be applied to
            // elements of list; not doing so for now
        else { // missed one
            static_assert(false);
            throw internal_error{"parser<CharT>::casted"};
        }
    }, val_var);
}

template <typename CharT>
inline bool parser<CharT>::identifiers_match(string_view inp_symb, const char* tab_symb) {
    return char_helper::eq(inp_symb, tab_symb);
    // may want to support case insensitive match in the future--maybe
}

template <typename CharT>
auto parser<CharT>::evaluate(const CharT* input) -> bool {
    lookahead_lexer lexer = {input, default_radix_};
    if (lexer.peek_tok().id == token::end) {
        last_val_ = 0.0;
        return false;
    }
    last_val_ = std::move(expression(lexer));
    if (lexer.get_tok().id != token::end)
        throw parse_error(parse_error::syntax_error, lexer.cached_tok());
    return true;
}

template <typename CharT>
auto parser<CharT>::expression(lookahead_lexer& lexer) -> parser_val_type {
// <expression> ::= <identifier> "=" ( <expression> | <end> ) | <arithmetic expr>
    parser_val_type val;
    lexer.peek_tok2();
    if (lexer.peeked_tok().id == token::identifier && lexer.peeked_tok2().id == token::eq) {
        auto key = lexer.get_tok().tok_str;
        lexer.get_tok(); // eat "="
        auto pos = vars_.find(key);
        if (lexer.peek_tok().id == token::end) {
        // erase the variable (if it's not already so)
            if (pos == vars_.end())
                val = std::numeric_limits<float_type>::quiet_NaN();
            else {
                val = std::move(pos->second.val_var);
                var_keys.erase(pos->second.key_pos); // erase corresponding entry in var_keys
                vars_.erase(pos);
            }
        } else if (pos == vars_.end()) {
        // variable not found; insert new one with <expression>
            val = std::move(expression(lexer));
            auto var_key_pos = var_keys.emplace(string{key.begin(), key.end()});
            vars_.try_emplace(*var_key_pos, variable{var_key_pos, val});
        } else
        // variable found; assign to it <expression>
            val = pos->second.val_var = std::move(expression(lexer));
        notify_vars_changed();
    } else
        val = std::move(arithmetic_expr(lexer));
    return val;
}

template <typename CharT>
auto parser<CharT>::arithmetic_expr(lookahead_lexer& lexer) -> parser_val_type {
// <arithmetic expr> ::= <bor term> [ "|" <bor term> ]...
    auto lval_num = std::move(bor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bor) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(bor_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval | rval);
                else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::bor_term(lookahead_lexer& lexer) -> parser_val_type {
// <bor term> ::= <bxor term> [ "^" <bxor term> ]...
    auto lval_num = std::move(bxor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bxor) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(bxor_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval ^ rval);
                else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::bxor_term(lookahead_lexer& lexer) -> parser_val_type {
// <bxor term> ::= <band term> [ "&" <band term> ]...
    auto lval_num = std::move(band_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::band) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(band_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval & rval);
                else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::band_term(lookahead_lexer& lexer) -> parser_val_type {
// <band term> ::= <shift term> [ ( "<<" | "<<<" | ">>" | ">>>" ) <shift term> ]...
    token op_tok;

    auto shift_arg_in_range = [&](const auto& lval, const auto& shift_arg) -> bool {
        // assume shift_arg is valid only if positive and less than # bits in
        // lval's parse_error. if shift_arg is negative then it's unusable;
        // parse_error will be thrown in that case. if shift_arg is >= # bits in
        // lval's type then we will simulate shifting beyond that limit.
        // precondition: lval and shift_arg are integral (integer) types
        using LVT = std::decay_t<decltype(lval)>;
        using ShiftT = std::decay_t<decltype(shift_arg)>;
        static_assert(std::is_integral_v<LVT>);
        static_assert(std::is_integral_v<ShiftT>);
        if constexpr (std::is_signed_v<ShiftT>)
            if (shift_arg < 0)
                throw parse_error(parse_error::negative_shift_invalid, op_tok);
        return shift_arg < (sizeof(LVT) * std::numeric_limits<unsigned char>::digits);
    };

    auto lval_num = std::move(shift_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::ashiftl) {
            op_tok = lexer.get_tok();
            auto rval_num = shift_term(lexer);
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
                    // arithmetic and logical left shift are equivalent
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<LVT>(0);
                    return static_cast<LVT>(lval << rval);
                } else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::ashiftr) {
            op_tok = lexer.get_tok();
            auto rval_num = shift_term(lexer);
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
                    // simulate arithmetic right shift incase real shift is logical one
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<LVT>(~static_cast<LVT>(0));
                    auto fill_bits = ~(~static_cast<LVT>(0) >> rval);
                    return static_cast<LVT>((lval >> rval) | fill_bits);
                } else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::lshiftl) {
            op_tok = lexer.get_tok();
            auto rval_num = shift_term(lexer);
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
                    // arithmetic and logical left shift are equivalent
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<LVT>(0);
                    return static_cast<LVT>(lval << rval);
                } else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::lshiftr) {
            op_tok = lexer.get_tok();
            auto rval_num = shift_term(lexer);
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
                    // arithmetic and logical right shift of unsigned type are equivalent
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<LVT>(0);
                    return static_cast<LVT>(std::make_unsigned_t<LVT>(lval) >> rval);
                } else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::shift_term(lookahead_lexer& lexer) -> parser_val_type {
// <shift term> ::= <term> [ ( "+" | "-" ) <term> ]...
    auto lval_num = std::move(term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::add) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(term(lexer));
            lval_num = apply_op(add_op(), lval_num, rval_num, error_context{op_tok});
        } else if (lexer.peeked_tok().id == token::sub) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(term(lexer));
            lval_num = apply_op(sub_op(), lval_num, rval_num, error_context{op_tok});
        } else {
            break;
        }
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::term(lookahead_lexer& lexer) -> parser_val_type {
// <term> ::= <factor> [ ( "*" | "/" | "%" | "." ) <factor> ]...
    auto lval_num = std::move(factor(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::mul) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_op(mul_op(), lval_num, rval_num, error_context{op_tok});
        } else if (lexer.peeked_tok().id == token::div) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_op(div_op(), lval_num, rval_num, error_context{op_tok});
        } else if (lexer.peek_tok().id == token::mod) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> parser_val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>) {
                    if (rval == 0)
                        throw parse_error(parse_error::division_by_0, op_tok);
                    return static_cast<LVT>(lval % rval);
                } else
                    throw parse_error(parse_error::int_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::dot) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> float_type {
                using LT = std::decay_t<decltype(lval)>;
                using RT = std::decay_t<decltype(rval)>;
                if constexpr (std::is_same_v<LT, list_type> && std::is_same_v<RT, list_type>) {
                    if (lval.size() != rval.size())
                        throw parse_error(parse_error::lists_must_be_same_size, op_tok);
                    auto lend = lval.begin() + lval.size();
	                auto ritr = rval.begin();
                    float_type sum = 0;
	                for (auto litr = lval.begin(); litr != lend; ++litr, ++ritr)
                        sum += std::visit([](auto lval, auto rval) -> float_type { // note: apply_promoted doesn't work with parser_num_type
                            return lval * rval;
                        }, *litr, *ritr);
                    return sum;
                } else
                    throw parse_error(parse_error::operands_must_be_lists, op_tok);
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::factor(lookahead_lexer& lexer) -> parser_val_type {
// <factor> ::= <unary expression> | <exponentiation>
// <unary expression> ::= ( "+" | "-" | "~" ) <factor>
// <exponentiation> ::= <primary expression> [ "**" <factor> ]...
// exponentiation is right-associative.
// exponentiation has higher precedence than unary negation (+/-).
// bitwise-not should probably have higher precedence than exponentiation but
// that would result in ~-1 being a syntax error, so it's here.
    if (lexer.peek_tok().id == token::add) { // unary +, basically do nothing
        auto op_tok = lexer.get_tok();
        auto val_num = std::move(factor(lexer));
        return std::visit([&](const auto& val) -> parser_val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_floating_point_v<VT> || std::is_integral_v<VT>)
                return val;
            else
                throw parse_error(parse_error::num_operand_expected, op_tok); 
        }, val_num);
    }
    if (lexer.peeked_tok().id == token::sub) { // negation
        auto op_tok = lexer.get_tok();
        auto val_num = std::move(factor(lexer));
        return std::visit([&](const auto& val) -> parser_val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_floating_point_v<VT> || std::is_signed_v<VT>)
                return static_cast<VT>(-val);
            else if constexpr (std::is_unsigned_v<VT>) // (note: MSVC++ doesn't like negating unsigned type)
                return static_cast<VT>(-static_cast<std::make_signed_t<VT>>(val));
            else
                throw parse_error(parse_error::num_operand_expected, op_tok);
        }, val_num);
    }
    if (lexer.peeked_tok().id == token::bnot) {
        auto op_tok = lexer.get_tok();
        auto val_num = std::move(factor(lexer));
        return std::visit([&](auto val) -> parser_val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_integral_v<VT>)
                return static_cast<VT>(~val);
            else
                throw parse_error(parse_error::int_operand_expected, op_tok);
        }, val_num);
    }

    auto lval_num = std::move(primary_expression(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::pow) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_op(exp_op(), lval_num, rval_num, error_context{op_tok});
        } else
            break;
    }
    return lval_num;
}

template <typename CharT>
auto parser<CharT>::primary_expression(lookahead_lexer& lexer) -> parser_val_type {
// <primary_expression> ::= ( <number> | <identifier> | <group> | <list> ) [ "!" | "!!" ]...
    parser_val_type lval;
    if (lexer.peek_tok().id == token::number) {
        static_assert(!std::is_same_v<parser_val_type, token::num_type>); // otherwise lval = casted(lexer.get_tok().num_val) would suffice
        auto num_val = lexer.get_tok().num_val;
        if (std::holds_alternative<token::int_type>(num_val))
            lval = std::get<token::int_type>(num_val);
        else {
            assert(std::holds_alternative<token::float_type>(num_val));
            lval = std::get<token::float_type>(num_val);
        }
        lval = casted(lval);
    } else if (lexer.peeked_tok().id == token::identifier)
        lval = std::move(identifier(lexer));
    else if (lexer.peeked_tok().id == token::lparen)
        lval = std::move(group(lexer));
    else if (lexer.peeked_tok().id == token::lsquare)
        lval = std::move(list(lexer));
    else if (lexer.peeked_tok().id == token::end)
        throw parse_error(parse_error::unexpected_end_of_input, lexer.peeked_tok());
    else
        throw parse_error(parse_error::syntax_error, lexer.peeked_tok());

    for (;;) {
        if (lexer.peek_tok().id == token::fac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> parser_val_type {
                using VT = std::decay_t<decltype(val)>;
                if constexpr (std::is_floating_point_v<VT> || std::is_integral_v<VT>)
                    return tgamma(val + 1);
                else
                    throw parse_error(parse_error::num_operand_expected, op_tok);
            }, lval);
        } else if (lexer.peek_tok().id == token::dfac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> parser_val_type {
                using VT = std::decay_t<decltype(val)>;
                if constexpr (std::is_floating_point_v<VT> || std::is_integral_v<VT>) {
                    float_type cpi = cos(pi * val);
                    return pow(2.0, (1.0 + 2.0 * val - cpi) / 4.0)
                        * pow(pi, (cpi - 1.0) / 4.0) * tgamma(1.0 + val / 2.0);
                } else
                    throw parse_error(parse_error::num_operand_expected, op_tok);
            }, lval);
        } else
            break;
    }

    return lval;
}

template <typename CharT>
auto parser<CharT>::identifier(lookahead_lexer& lexer) -> parser_val_type {
// <identifier> ::= <variable> | <unary_fn> | <list fn> | <internal value>
// <unary fn> ::= <unary fn name> <group>
// <list fn> ::= <list fn name> <list>
    lexer.get_expected_tok(token::identifier);

    // <variable>
    if (auto pos = vars_.find(lexer.cached_tok().tok_str); pos != vars_.end())
        return pos->second.val_var;

    // <unary fn name>
    for (auto pos = unary_fn_table.begin(); pos != unary_fn_table.end(); ++pos)
        if (identifiers_match(lexer.cached_tok().tok_str, pos->first))
            return pos->second(get_as<float_type>(group(lexer)));

    // <list fn name>
    for (auto pos = list_fn_table.begin(); pos != list_fn_table.end(); ++pos)
        if (identifiers_match(lexer.cached_tok().tok_str, pos->first))
            return pos->second(list(lexer));

    // <internal value>
    if (identifiers_match(lexer.cached_tok().tok_str, "pi"))
        return pi;
    if (identifiers_match(lexer.cached_tok().tok_str, "e"))
        return e;
    if (identifiers_match(lexer.cached_tok().tok_str, "last"))
        return last_val_;

    throw parse_error(parse_error::undefined_identifier, lexer.cached_tok());
};

template <typename CharT>
auto parser<CharT>::group(lookahead_lexer& lexer) -> parser_val_type {
// <group> ::= '(' <expression> ')'
    lexer.get_expected_tok(token::lparen);
    parser_val_type val = std::move(expression(lexer));
    lexer.get_expected_tok(token::rparen);
    return val;
}

template <typename CharT>
auto parser<CharT>::list(lookahead_lexer& lexer) -> list_type {
// <list> ::= '[' <expression> [ ',' <expression> ]... ']'
    auto tok = lexer.get_expected_tok(token::lsquare);
    list_type list;
    for (;;) {
        std::visit([&](const auto& val) {
            if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
                list.insert(list.end(), val.begin(), val.end());
            else
                list.emplace_back(val);
        }, std::move(expression(lexer)));
        if (lexer.peek_tok().id != token::comma)
            break;
        tok = lexer.get_tok();
    }

    lexer.get_expected_tok(token::rsquare);
    return list;
}

template <typename CharT>
std::array<std::pair<const char*, typename parser<CharT>::unary_fn>, 20> parser<CharT>::unary_fn_table = {{
    {"exp", exp}, // e ** n
    {"log", log}, // natural (base e) log
    {"log10", log10}, // base 10 log
    {"log2", log2}, // base 2 log
    {"sqrt", sqrt},
    {"cbrt", cbrt}, // cubic root of n
    {"sin", sin},
    {"cos", cos},
    {"tan", tan},
    {"asin", asin}, // arc sin
    {"acos", acos}, // arc cos
    {"atan", atan}, // arc tan
    {"sinh", sinh}, // hyperbolic sin
    {"cosh", cosh}, // hyperbolic cos
    {"tanh", tanh}, // hyperbolic tan
    {"asinh", asinh}, // inverse hyperbolic sin
    {"acosh", acosh}, // inverse hyperbolic cos
    {"atanh", atanh}, // inverse hyperbolic tan
    {"gamma", tgamma}, // gamma
    {"lgamma", lgamma}, // log gamma
}};

template <typename CharT>
std::array<std::pair<const char*, typename parser<CharT>::list_fn>, 21> parser<CharT>::list_fn_table = {{
    {"sum", sum},
    {"prod", prod},
    {"avg", avg},
    {"geomean", geomean},
    {"harmmean", harmmean},
    {"variance", variance},
    {"stddev", stddev},
    {"pvariance", pvariance},
    {"pstddev", pstddev},
    {"median", median},
    {"mode", mode},
    {"min", min},
    {"max", max},
    {"quartile1", quartile1},
    {"quartile2", quartile2},
    {"quartile3", quartile3},
    {"iqr", iqr},
    {"range", range},
    {"madmean", madmean},
    {"madmed", madmed},
    {"qdev", qdev}
}};

template <typename CharT>
auto parser<CharT>::sum(const list_type& list) -> parser_val_type {
    float_type val = 0;
    for (auto& list_val : list)
        val += get_as<float_type>(list_val);
    return val;
}

template <typename CharT>
auto parser<CharT>::prod(const list_type& list) -> parser_val_type {
    float_type val = 1;
        // note: product of empty list (empty set) is 1; see
        // https://en.wikipedia.org/wiki/Empty_product
    for (auto& list_val : list)
        val *= get_as<float_type>(list_val);
    return val;
}

template <typename CharT>
auto parser<CharT>::avg(const list_type& list) -> parser_val_type {
    float_type val = 0;
    for (auto& list_val : list)
        val += get_as<float_type>(list_val);
    return val / list.size();
}

template <typename CharT>
auto parser<CharT>::geomean(const list_type& list) -> parser_val_type {
    float_type val = 1;
    for (auto& list_val : list)
        val *= get_as<float_type>(list_val);
    return pow(val, 1.0 / list.size());
}

template <typename CharT>
auto parser<CharT>::harmmean(const list_type& list) -> parser_val_type {
    float_type val = 0;
    for (auto& list_val : list)
        val += 1.0 / get_as<float_type>(list_val);
    return list.size() / val;
}

template <typename CharT>
inline auto parser<CharT>::variance(const list_type& list, typename list_type::size_type n_adjustment) -> float_type {
// n_adjustment: 0 for population variance, 1 for sample variance
    if (n_adjustment != 0 && n_adjustment != 1) {
        assert(false);
        throw internal_error{"parser<CharT>::variance"};
    }

    if (!list.size())
        return std::numeric_limits<float_type>::infinity();

    // Welford's online method:
    float_type mean = 0;
    float_type sum = 0;
    typename list_type::size_type i = 1;
    for (auto pval = list.begin(); pval != list.end(); ++pval, ++i) {
        auto x = get_as<float_type>(*pval);
        auto delta = (x - mean);
        mean += delta / i;
        sum += delta * (x - mean);
    }
    return sum / (list.size() - n_adjustment);
}

template <typename CharT>
auto parser<CharT>::variance(const list_type& list) -> parser_val_type {
    return variance(list, 1);
}

template <typename CharT>
auto parser<CharT>::stddev(const list_type& list) -> parser_val_type {
    return sqrt(variance(list, 1));
}

template <typename CharT>
auto parser<CharT>::pvariance(const list_type& list) -> parser_val_type {
    return variance(list, 0);
}

template <typename CharT>
auto parser<CharT>::pstddev(const list_type& list) -> parser_val_type {
    return sqrt(variance(list, 0));
}

template <typename CharT>
inline auto parser<CharT>::quantile(const list_type& list, float_type percent) -> float_type {
    // precondition: (0 <= percent <= 1)
    assert(percent >= 0);
    assert(percent <= 1);
    if (percent < 0 || percent > 1)
        throw internal_error{"parser<CharT>::quantile"};

    if (!list.size())
        return std::numeric_limits<float_type>::quiet_NaN();
    if (list.size() == 1)
        return get_as<float_type>(list.front());

    list_type list_;
    list_.reserve(list.size());
    for (auto& val: list)
        list_.emplace_back(get_as<float_type>(val));
    std::sort(list_.begin(), list_.end());

    float_type fidx = percent * static_cast<float_type>(list_.size() + 1) - 1.0f;
    auto idx = static_cast<typename list_type::size_type>(fidx); // truncate to integer
    if (idx == list_.size()) // percent is 1 (not testing percent directly in case of precision error)
        return get_as<float_type>(list_.back());

    assert(list_.size() > 1);
    auto x0 = get_as<float_type>(list_[idx]);
    auto x1 = get_as<float_type>(list_[idx + 1]);
    float_type interpolation_factor = fidx - idx;
    return x0 + interpolation_factor * (x1 - x0);
}

template <typename CharT>
auto parser<CharT>::median(const list_type& list) -> parser_val_type {
    return quantile(list, 0.5);
}

template <typename CharT>
auto parser<CharT>::mode(const list_type& list) -> parser_val_type {
    if (list.size() < 2) // no possible modes, also for assumption below
        return list_type(); // want to return empty list rather than nan here

    list_type list_;
    list_.reserve(list.size());
    for (auto& val: list)
        list_.emplace_back(get_as<float_type>(val));
    std::sort(list_.begin(), list_.end());

    using int_type = typename list_type::size_type;
    std::vector<int_type> counts; // counts will be in correspondance with list_
    counts.reserve(list_.size());
    int_type high_count = 2; // 2 to exclude modes that would be just a single item

    int_type count = 1;
    assert(list_.size() > 1);
    for (int_type idx = 1; idx <= list_.size(); ++idx) {
        counts.emplace_back(count);
        if (idx < list_.size() && list_[idx - 1] == list_[idx])
            ++count; // will be added to counts on next iteration
        else {
            if (count > high_count)
                high_count = count;
            count = 1;
        }
    }
    
    assert(counts.size() == list_.size());
    int_type list_idx = 0;
    for (auto itr = counts.begin(); itr != counts.end(); ++itr)
        if (*itr == high_count)
            ++list_idx;
        else
            list_.erase(list_.begin() + list_idx);
    
    if (list_.size() == 1)
        return list_.front();
    return list_;
}

template <typename CharT>
auto parser<CharT>::min(const list_type& list) -> parser_val_type {
    if (!list.size())
        return std::numeric_limits<float_type>::quiet_NaN();
    auto min = get_as<float_type>(list.front());
    for (auto itr = list.begin() + 1; itr != list.end(); ++itr) {
        float_type item = get_as<float_type>(*itr);
        if (item < min)
            min = item;
    }
    return min;
}

template <typename CharT>
auto parser<CharT>::max(const list_type& list) -> parser_val_type {
    if (!list.size())
        return std::numeric_limits<float_type>::quiet_NaN();
    float_type max = get_as<float_type>(list.front());
    for (auto itr = list.begin() + 1; itr != list.end(); ++itr) {
        float_type item = get_as<float_type>(*itr);
        if (item > max)
            max = item;
    }
    return max;
}

template <typename CharT>
auto parser<CharT>::quartile1(const list_type& list) -> parser_val_type {
    return quantile(list, 0.25);
}

template <typename CharT>
auto parser<CharT>::quartile2(const list_type& list) -> parser_val_type {
    return quantile(list, 0.5); // equivalent to median
}

template <typename CharT>
auto parser<CharT>::quartile3(const list_type& list) -> parser_val_type {
    return quantile(list, 0.75);
}

template <typename CharT>
auto parser<CharT>::iqr(const list_type& list) -> parser_val_type {
    return quantile(list, 0.75) - quantile(list,0.25);
}

template <typename CharT>
auto parser<CharT>::range(const list_type& list) -> parser_val_type {
    if (!list.size())
        return std::numeric_limits<float_type>::quiet_NaN();
    float_type max = get_as<float_type>(list.front());
    float_type min = get_as<float_type>(list.front());
    for (auto itr = list.begin() + 1; itr != list.end(); ++itr) {
        float_type item = get_as<float_type>(*itr);
        if (item < min)
            min = item;
        if (item > max)
            max = item;
    }
    return max - min;
}

template <typename CharT>
auto parser<CharT>::madmean(const list_type& list) -> parser_val_type {
    auto mean = get_as<float_type>(avg(list));
    float_type sum = 0;
    for (auto& list_val : list)
        sum += fabs(get_as<float_type>(list_val) - mean);
    return sum / list.size(); // inf for empty list
}

template <typename CharT>
auto parser<CharT>::madmed(const list_type& list) -> parser_val_type {
    auto med = get_as<float_type>(median(list));
    list_type list_;
    list_.reserve(list.size());
    for (auto& list_val : list)
        list_.emplace_back(fabs(get_as<float_type>(list_val) - med));
    return median(list_);
}

template <typename CharT>
auto parser<CharT>::qdev(const list_type& list) -> parser_val_type {
    return (quantile(list, 0.75) - quantile(list, 0.25)) / 2;
}

} // namespace parser

#endif // CALC_PARSER_H
