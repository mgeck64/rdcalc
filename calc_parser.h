#pragma once
#ifndef CALC_PARSER_H
#define CALC_PARSER_H

#include "calc_lexer.h"
#include <map>
#include <set>
#include <type_traits>
#include <functional>
#include <vector>
#include <algorithm>

template <typename CharT> // CharT: char or wchar_t
class calc_parser {
public:
    using token = token<CharT>;
    using token_ids = typename token::token_ids;
    using token_error_codes = typename token::error_codes;
    using string_view = std::basic_string_view<CharT>;
    using string = std::basic_string<CharT>;
    using float_type = typename token::float_type;
    using radices = typename token::radices;
    
    using notify_vars_changed_fn = std::function<void()>;
    calc_parser(const notify_vars_changed_fn& notify_vars_changed_)
        : notify_vars_changed{notify_vars_changed_} {}

    calc_parser() = default;

    calc_parser(const calc_parser&) = delete;
    calc_parser& operator=(calc_parser&) = delete;
    // disable copy/assignment; don't think can trivially copy vars_ below
    // because of how its elements bind to var_keys

    using num_type = std::variant<
        std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
        std::uint32_t, std::int64_t, std::uint64_t, float_type>;
        // in order of ascending promotability

    using list_type = std::vector<num_type>;

    using val_type_base = std::variant<
        std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
        std::uint32_t, std::int64_t, std::uint64_t, float_type, list_type>;
        // elements correspond with num_type (except for list_type)

    struct val_type : val_type_base {
        template <typename T> // for converting num_type alternative to val_type
        val_type(const T& val) : val_type_base{val} {}

        val_type(const num_type& num_val) { // convert num_type to val_type
            std::visit([&](const auto& val) {
                *this = val;
            }, num_val);
        }

        val_type() = default;
    };

    static constexpr auto num_type_short_txt = std::array
        // short text for UI.
        // elements correspond with val_type variant so index() can be used as index
        {"Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Flt. Pt.", "List"};
    using widest_uint_type = std::uint64_t;
    using widest_int_type = std::int64_t;

    enum int_result_tags : unsigned {
        int8_tag, uint8_tag, int16_tag, uint16_tag, int32_tag, uint32_tag, int64_tag, uint64_tag};
        // must correspond with val_type's alternative integer types
    auto int_result_tag() const -> auto {return int_result_tag_;}
    void int_result_tag(int_result_tags int_result_tag);

    struct parse_error { // exception
        enum error_codes {
            no_error, lexer_error, syntax_error, num_val_expected,
            undefined_identifier, tok_expected,
            num_lval_expected, num_rval_expected,
            int_lval_expected, int_rval_expected,
            rval_negative_invalid, division_by_0, unexpected_error,
            nested_list_invalid};
        static constexpr auto error_txt = std::array{
            // elements correspond with error_codes enums so enum can be used as index
            "no_error", "lexer error", "syntax error", "number expected",
            "is undefined", "was expected",
            "number expected for left operand", "number expected for right operand",
            "integer expected for left operand (check mode)", "integer expected for right operand",
            "negative right operand invalid", "division by 0", "unexpected error",
            "nested list at left is invalid"};
        error_codes error = no_error;
        token tok; // warning: has string_view that binds to the input string
        token_ids expected_tok = token::none; // valid for error == tok_expected
        void assert_view_is_valid_for(const CharT* input) const;
        auto error_str() const -> string;
    };

    using error_codes = typename parse_error::error_codes;

    auto default_radix() -> radices {return default_radix_;}
    auto default_radix(radices default_radix) -> void {default_radix_ = default_radix;}

    auto eval(const CharT* input) -> bool;
    // evaluates the input string; throws parse_error on parsing error. input is
    // as specified for calc_lexer. uses a calc_lexer object to which input is
    // assigned (see); the calc_lexer object persists only for duration of this
    // function call. returns false of the input string was all empty or all
    // whitespace; true otherwise.

    auto last_val() const -> val_type;
    // returns last evaluated result

    using var_keys_type = std::multiset<string>;
    struct variable {
        const typename var_keys_type::iterator key_pos; // see var_keys below
        val_type num_val;
    };
    using vars_type = std::map<string_view, variable>;
    const vars_type& vars() const {return vars_;}

private:
    class lookahead {
    // simulates two-token lookhead lexer using calc_lexer. (implemented
    // separately from calc_lexer to keep calc_lexer clean and simple, and to
    // encapsulate lookahead logic)
        calc_lexer<CharT> lexer;
        unsigned peeked = 0;
        token peeked_tok_ = {};
        token peeked_tok2_ = {};
        token cached_tok_ = {};

    public:
        lookahead(const CharT* input, const radices& default_radix) :
            lexer{input, default_radix} {}
        lookahead() = default;

        auto get_tok() -> const token&; // consume a token; throws parse_error if token has error
        auto cached_tok() const -> const token& {return cached_tok_;}

        auto get_expected_tok(token_ids id) -> const token&;
        // calls get_tok() to consume a token; throws parse_error if id doesn't
        // match consumed token's id

        auto peek_tok() -> const token&; // peek at but don't consume token
        auto peeked_tok() const -> const token& {return peeked_tok_;}

        auto peek_tok2() -> const token&; // peek at second token
        auto peeked_tok2() const -> const token& {return peeked_tok2_;}
    };

    radices default_radix_ = radices::decimal;
    int_result_tags int_result_tag_ = int64_tag;

    val_type last_val_ = 0.0;
    // result of last evaluation of non-empty input, accessable in an input
    // expression as "last"

    vars_type vars_;
    // key is string_view to allow direct and efficient comparison with
    // token::tok_str

    var_keys_type var_keys;
    // keys need to persist between eval() calls so copies will be stored here;
    // keys in vars_ will be views of strings stored here. variable::key_pos
    // points to corresponding entry here so it can be deleted when entry in
    // vars_ is deleted

    // a crucial reason vars_ and var_keys are map/multiset respectively is
    // because iterators need to remain valid after changes are made to the
    // containers; this eliminates vector despite the appeal of its simplicity

    template <typename T>
    static auto get_as(const val_type& num_val) -> T;
    // returns num_val with its value casted as T; precondition: T is one of
    // val_type's alternative types

    static auto get_as(const size_t& idx, const val_type& num_val) -> val_type;
    // returns num_val with it's value casted as the alternative type indicated
    // by idx (index)

    auto casted(const val_type& num_val) const -> val_type;
    // if num_val is an integer type then this casts it to the type indicated by
    // int_result_tag_; else (for float_type) this simply returns num_val

    static auto promoted(const val_type& lnum_val, const val_type& rnum_val) -> std::pair<val_type, val_type>;
    // casts either lnum_val's or rnum_val's alternative to the higher promotion
    // level of the two and returns the modified value along with the other as a
    // pair

    template <typename Fn>
    static auto apply_promoted(const Fn& fn, const val_type& lnum_val, const val_type& rnum_val) -> auto;
    // fn: functor/lambda with operator()(T, T) -> val_type overloaded for each
    // of val_type's alternetive types (T). applies promoted() to lnum_val and
    // rnum_val, then applies fn() to lnum_val's and rnum_val's promoted values.
    // this function is used instead of std:visit to avoid generating
    // exponential number of functions, which can be done because promoted()
    // insures lnum_val and rnum_val will have the same alternative type

    static void validate_num_type(const val_type& rval, const val_type& lval, const token& op_tok);
    static void validate_int_only(const val_type& rval, const val_type& lval, const token& op_tok);
    
    static auto make_parse_error(const error_codes& error, const token& tok, token_ids expected_tok_id = token::unspecified) -> parse_error;
    // expected_tok is only valid for error == parse_error::tok_expected

    // parser productions
    auto expression(lookahead& lexer) -> val_type;
    auto arithmetic_expr(lookahead& lexer)-> val_type;
    auto bor_term(lookahead& lexer) -> val_type;
    auto bxor_term(lookahead& lexer) -> val_type;
    auto band_term(lookahead& lexer) -> val_type;
    auto shift_term(lookahead& lexer) -> val_type;
    auto term(lookahead& lexer)-> val_type;
    auto factor(lookahead& lexer)-> val_type;
    auto primary_expression(lookahead& lexer)-> val_type;
    auto identifier(lookahead& lexer)-> val_type;
    auto group_or_list(lookahead& lexer, size_t expected_count = 0)-> val_type;

    using unary_fn = float_type (*)(float_type);
    static std::array<std::pair<const char*, unary_fn>, 20> unary_fn_table;
    // unary_fn_table: simple unordered array; expected to be small enough that
    // simple linear search will be adequate

    using list_fn = val_type (*)(const val_type&);
    static std::array<std::pair<const char*, list_fn>, 6> list_fn_table;
    // list_fn_table: simple unordered array; expected to be small enough that
    // simple linear search will be adequate

    static bool identifiers_match(string_view inp_symb, const char* tab_symb);
    // input identifier matches identifier in table?
    // internally, identifier in table is const char* regardless of CharT

    static auto sum(const val_type& num_val) -> val_type;
    static auto prod(const val_type& num_val) -> val_type;
    static auto avg(const val_type& num_val) -> val_type;
    static auto variance(const val_type& num_val) -> val_type;
    static auto stddev(const val_type& num_val) -> val_type;
    static auto median(const val_type& num_val) -> val_type;

    static auto sum(const list_type& list) -> val_type;
    static auto prod(const list_type& list) -> val_type;
    static auto avg(const list_type& list) -> val_type;
    static auto variance(const list_type& list) -> val_type;
    static auto stddev(const list_type& list) -> val_type;
    static auto median(const list_type& list) -> val_type;

    static constexpr auto pi = 3.14159265358979323846;
    static constexpr auto e = 2.71828182845904523536;

    notify_vars_changed_fn notify_vars_changed = []{}; // initialize to do-nothing lambda
};

#include "parse_error.h"
#include "lookahead.h"

template <typename CharT>
inline auto calc_parser<CharT>::last_val() const -> val_type {
    return last_val_;
}

// TODO: am casting between int types with, in many cases, undefined or
// implementation defined behavior; should replace that with own well-defined
// casts but don't have time to do that right now so will live with MSVC++'s
// implemented behavior

template <typename CharT>
void calc_parser<CharT>::int_result_tag(int_result_tags int_result_tag) {
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
//        var.num_val = casted(var.num_val);
}

template <typename CharT>
template <typename T>
auto calc_parser<CharT>::get_as(const val_type& num_val) -> T {
    // precondition: any of val_type's types is convertable to T
    switch (num_val.index()) {
    case 0: return static_cast<T>(*std::get_if<0>(&num_val));
    case 1: return static_cast<T>(*std::get_if<1>(&num_val));
    case 2: return static_cast<T>(*std::get_if<2>(&num_val));
    case 3: return static_cast<T>(*std::get_if<3>(&num_val));
    case 4: return static_cast<T>(*std::get_if<4>(&num_val));
    case 5: return static_cast<T>(*std::get_if<5>(&num_val));
    case 6: return static_cast<T>(*std::get_if<6>(&num_val));
    case 7: return static_cast<T>(*std::get_if<7>(&num_val));
    case 8: return static_cast<T>(*std::get_if<8>(&num_val));
    default:
        // list_type or missed one; list_type case should have been handled in
        // higher-level code
        assert(false);
        throw make_parse_error(parse_error::unexpected_error, {});
    }
}

template <typename CharT>
auto calc_parser<CharT>::get_as(const size_t& idx, const val_type& num_val) -> val_type {
    if (idx == num_val.index())
        return num_val;
    val_type num_val_;
    switch (idx) {
    case 0: num_val_ = get_as<std::int8_t>(num_val); break;
    case 1: num_val_ = get_as<std::uint8_t>(num_val); break;
    case 2: num_val_ = get_as<std::int16_t>(num_val); break;
    case 3: num_val_ = get_as<std::uint16_t>(num_val); break;
    case 4: num_val_ = get_as<std::int32_t>(num_val); break;
    case 5: num_val_ = get_as<std::uint32_t>(num_val); break;
    case 6: num_val_ = get_as<std::int64_t>(num_val); break;
    case 7: num_val_ = get_as<std::uint64_t>(num_val); break;
    case 8: num_val_ = get_as<float_type>(num_val); break;
    default:
        // list_type or missed one; list_type case should have been handled in
        // higher-level code
        assert(false);
        throw make_parse_error(parse_error::unexpected_error, {});
    }
    assert(idx == num_val_.index());
    return num_val_;
}

template <typename CharT>
auto calc_parser<CharT>::casted(const val_type& num_val) const -> val_type {
    return std::visit([&](const auto& val) -> val_type {
        using VT = std::decay_t<decltype(val)>;
        if constexpr (std::is_integral_v<VT>)
            return get_as(int_result_tag_, num_val);
        else if constexpr (std::is_same_v<VT, float_type> || std::is_same_v<VT, list_type>)
            return val;
            // TODO: for list_type, not sure if casted should be applied to
            // elements of list; not doing so for now
        else { // missed one
            static_assert(false);
            throw make_parse_error(parse_error::unexpected_error, {});
        }
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::promoted(const val_type& lnum_val, const val_type& rnum_val) -> std::pair<val_type, val_type> {
    if (lnum_val.index() < rnum_val.index())
        return {get_as(rnum_val.index(), lnum_val), rnum_val};
    if (lnum_val.index() > rnum_val.index())
        return {lnum_val, get_as(lnum_val.index(), rnum_val)};
    return {lnum_val, rnum_val};
}

template <typename CharT>
template <typename Fn>
inline auto calc_parser<CharT>::apply_promoted(const Fn& fn, const val_type& lnum_val, const val_type& rnum_val) -> auto {
    auto [lnum_val_, rnum_val_] = std::move(promoted(lnum_val, rnum_val));
    assert(lnum_val_.index() == rnum_val_.index());
    switch (lnum_val_.index()) {
    case 0: return fn(*std::get_if<0>(&lnum_val_), *std::get_if<0>(&rnum_val_));
    case 1: return fn(*std::get_if<1>(&lnum_val_), *std::get_if<1>(&rnum_val_));
    case 2: return fn(*std::get_if<2>(&lnum_val_), *std::get_if<2>(&rnum_val_));
    case 3: return fn(*std::get_if<3>(&lnum_val_), *std::get_if<3>(&rnum_val_));
    case 4: return fn(*std::get_if<4>(&lnum_val_), *std::get_if<4>(&rnum_val_));
    case 5: return fn(*std::get_if<5>(&lnum_val_), *std::get_if<5>(&rnum_val_));
    case 6: return fn(*std::get_if<6>(&lnum_val_), *std::get_if<6>(&rnum_val_));
    case 7: return fn(*std::get_if<7>(&lnum_val_), *std::get_if<7>(&rnum_val_));
    case 8: return fn(*std::get_if<8>(&lnum_val_), *std::get_if<8>(&rnum_val_));
    default:
        // list_type or missed one; list_type case should have been handled in
        // higher-level code
        assert(false);
        throw make_parse_error(parse_error::unexpected_error, {});
    }
}

template <typename CharT>
inline auto calc_parser<CharT>::make_parse_error(const error_codes& error, const token& tok, token_ids expected_tok_id) -> parse_error {
    return parse_error{error, tok, expected_tok_id};
}

template <typename CharT>
inline bool calc_parser<CharT>::identifiers_match(string_view inp_symb, const char* tab_symb) {
    return char_helper::eq(inp_symb, tab_symb);
    // may want to support case insensitive match in the future--maybe
}

template <typename CharT>
inline void calc_parser<CharT>::validate_num_type(const val_type& lnum_val, const val_type& rnum_val, const token& op_tok) {
    std::visit([&](const auto& lval) {
        using VT = std::decay_t<decltype(lval)>;
        if constexpr (!std::is_same_v<VT, float_type> && !std::is_integral_v<VT>)
            throw make_parse_error(parse_error::num_lval_expected, op_tok);
    }, lnum_val);
    std::visit([&](const auto& rval) {
        using VT = std::decay_t<decltype(rval)>;
        if constexpr (!std::is_same_v<VT, float_type> && !std::is_integral_v<VT>)
            throw make_parse_error(parse_error::num_rval_expected, op_tok);
    }, rnum_val);
    // lnum_val and rnum_val are visied independently to avoid generating
    // exponential number of lambdas
}

template <typename CharT>
inline void calc_parser<CharT>::validate_int_only(const val_type& lnum_val, const val_type& rnum_val, const token& op_tok) {
    std::visit([&](const auto& lval) {
        using VT = std::decay_t<decltype(lval)>;
        if constexpr (!std::is_integral_v<VT>)
            throw make_parse_error(parse_error::int_lval_expected, op_tok);
    }, lnum_val);
    std::visit([&](const auto& rval) {
        using VT = std::decay_t<decltype(rval)>;
        if constexpr (!std::is_integral_v<VT>)
            throw make_parse_error(parse_error::int_rval_expected, op_tok);
    }, rnum_val);
    // lnum_val and rnum_val are visied independently to avoid generating
    // exponential number of lambdas
}

template <typename CharT>
auto calc_parser<CharT>::eval(const CharT* input) -> bool {
    lookahead lexer = {input, default_radix_};
    if (lexer.peek_tok().id == token::end) {
        last_val_ = 0.0;
        return false;
    }
    last_val_ = std::move(expression(lexer));
    if (lexer.get_tok().id != token::end)
        throw make_parse_error(parse_error::syntax_error, lexer.cached_tok());
    return true;
}

template <typename CharT>
auto calc_parser<CharT>::expression(lookahead& lexer) -> val_type {
// <expression> ::= <identifier> "=" ( <expression> | <end> ) | <arithmetic expr>
    val_type val;
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
                val = std::move(pos->second.num_val);
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
            val = pos->second.num_val = std::move(expression(lexer));
        notify_vars_changed();
    } else
        val = std::move(arithmetic_expr(lexer));
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::arithmetic_expr(lookahead& lexer) -> val_type {
// <arithmetic expr> ::= <bor term> [ "|" <bor term> ]...
    auto lval = std::move(bor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bor) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(bor_term(lexer));
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval | rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else
            break;
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::bor_term(lookahead& lexer) -> val_type {
// <bor term> ::= <bxor term> [ "^" <bxor term> ]...
    auto lval = std::move(bxor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bxor) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(bxor_term(lexer));
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval ^ rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else
            break;
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::bxor_term(lookahead& lexer) -> val_type {
// <bxor term> ::= <band term> [ "&" <band term> ]...
    auto lval = std::move(band_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::band) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(band_term(lexer));
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval & rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else
            break;
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::band_term(lookahead& lexer) -> val_type {
// <band term> ::= <shift term> [ ( "<<" | "<<<" | ">>" | ">>>" ) <shift term> ]...
    token op_tok;

    // all this complexity is to try to insure consistent well-defined behavior.
    // for a << b or a >> b, the only behavior that's well-defined in standard
    // C++ is where a and b are positive/unsigned and b < number of bits in a.

    auto shift_arg_in_range = [&](const auto& lval, const auto& shift_arg) -> bool {
        // assume shift_arg is valid only if positive and less than # bits in lval's type
        // precondition: lval and shift_arg are integral (integer) types
        using LVT = std::decay_t<decltype(lval)>;
        using ShiftT = std::decay_t<decltype(shift_arg)>;
        static_assert(std::is_integral_v<LVT>);
        static_assert(std::is_integral_v<ShiftT>);
        if constexpr (std::is_signed_v<ShiftT>)
            if (shift_arg < 0)
                throw make_parse_error(parse_error::rval_negative_invalid, op_tok);
        return shift_arg < (sizeof(LVT) * CHAR_BIT);
    };

    auto lshiftl_op = [](const auto& lval, const auto& rval) -> auto { // logical shift left
        using LVT = std::decay_t<decltype(lval)>;
        return static_cast<LVT>(std::make_unsigned_t<LVT>(lval) << rval);
    };
    auto lshiftr_op = [](const auto& lval, const auto& rval) -> auto { // logical shift right
        using LVT = std::decay_t<decltype(lval)>;
        return static_cast<LVT>(std::make_unsigned_t<LVT>(lval) >> rval);
    };
    auto bnot_op = [](auto val) -> auto { // bitwise negation cast back as original type (to avoid promotion of, e.g., 8 bit type to int)
        return static_cast<std::decay_t<decltype(val)>>(~val);
    };

    auto logical_shift = [&](const auto& lval, const auto& rval, const auto& shift_op) -> val_type { // arithmetic and logical shift are the same in most cases
        using LVT = std::decay_t<decltype(lval)>;
        using RVT = std::decay_t<decltype(rval)>;
        if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
            if (!shift_arg_in_range(lval, rval))
                return static_cast<LVT>(0);
            return shift_op(lval, rval);
        } else {
            assert(false); // should never be invoked
            throw make_parse_error(parse_error::unexpected_error, op_tok);
        }
    };

    auto lshiftl = [&](const auto& lval, const auto& rval) -> val_type { // logical shift left
        return logical_shift(lval, rval, lshiftl_op);
    };

    auto lshiftr = [&](const auto& lval, const auto& rval) -> val_type { // logical shift right
        return logical_shift(lval, rval, lshiftr_op);
    };

    auto ashiftl = [&](const auto& lval, const auto& rval) -> val_type { // arithmetic shift left; equivalent to logical shift left
        return logical_shift(lval, rval, lshiftl_op);
    };

    auto ashiftr = [&](const auto& lval, const auto& rval) -> val_type { // arithmetic shift right
        using LVT = std::decay_t<decltype(lval)>;
        using RVT = std::decay_t<decltype(rval)>;
        if constexpr (std::is_integral_v<LVT> && std::is_integral_v<RVT>) {
            if constexpr (std::is_signed_v<LVT>)
                if (lval < 0) {
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<LVT>(~static_cast<LVT>(0));
                    auto fill_bits = bnot_op(lshiftr_op(bnot_op(static_cast<LVT>(0)), rval));
                    return static_cast<LVT>(lshiftr_op(lval, rval) | fill_bits);
                }
            assert(lval >= 0);
            if (!shift_arg_in_range(lval, rval))
                return static_cast<LVT>(0);
            return lshiftr_op(lval, rval);
        } else {
            assert(false); // should never be invoked
            throw make_parse_error(parse_error::unexpected_error, op_tok);
        }
    };

    auto lval = std::move(shift_term(lexer));
    auto do_shift = [&](const auto& shift_fn) {
        auto op_tok = lexer.get_tok();
        auto rval = shift_term(lexer);
        validate_int_only(lval, rval, op_tok);
        lval = std::visit(shift_fn, lval, rval);
    };
    for (;;) {
        if (lexer.peek_tok().id == token::ashiftl)
            do_shift(ashiftl);
        else if (lexer.peeked_tok().id == token::ashiftr)
            do_shift(ashiftr);
        else if (lexer.peeked_tok().id == token::lshiftl)
            do_shift(lshiftl);
        else if (lexer.peeked_tok().id == token::lshiftr)
            do_shift(lshiftr);
        else
            break;
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::shift_term(lookahead& lexer) -> val_type {
// <shift term> ::= <term> [ ( "+" | "-" ) <term> ]...
    auto lval = std::move(term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::add) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(term(lexer));
            validate_num_type(lval, rval, op_tok);
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_same_v<LVT, float_type> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval + rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else if (lexer.peeked_tok().id == token::sub) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(term(lexer));
            validate_num_type(lval, rval, op_tok);
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_same_v<LVT, float_type> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval - rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::term(lookahead& lexer) -> val_type {
// <term> ::= <factor> [ ( "*" | "/" | "%" ) <factor> ]...
    auto lval = std::move(factor(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::mul) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(factor(lexer));
            validate_num_type(lval, rval, op_tok);
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_same_v<LVT, float_type> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval * rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else if (lexer.peeked_tok().id == token::div) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(factor(lexer));
            validate_num_type(lval, rval, op_tok);
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>) {
                    if (rval == 0)
                        throw make_parse_error(parse_error::division_by_0, op_tok);
                    return static_cast<LVT>(lval / rval);
                }
                else if constexpr (std::is_same_v<LVT, float_type>)
                    // for floating point, let division by 0 resolve to inf
                    return static_cast<LVT>(lval / rval);
                else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else if (lexer.peek_tok().id == token::mod) {
            auto op_tok = lexer.get_tok();
            auto rval = std::move(factor(lexer));
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_integral_v<LVT>) {
                    if (rval == 0)
                        throw make_parse_error(parse_error::division_by_0, op_tok);
                    return static_cast<LVT>(lval % rval);
                } else { // should never be invoked
                    assert(false);
                    throw make_parse_error(parse_error::unexpected_error, op_tok);
                }
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::factor(lookahead& lexer) -> val_type {
// <factor> ::= <unary expression> | <exponentiation>
// <unary expression> ::= ( "+" | "-" | "~" ) <factor>
// <exponentiation> ::= <primary expression> [ "**" <factor> ]...
// exponentiation is right-associative.
// exponentiation has higher precedence than unary negation (+/-).
// bitwise-not should probably have higher precedence than exponentiation but
// that would result in ~-1 being a syntax error, so it's here.
    if (lexer.peek_tok().id == token::add) { // unary +, basically do nothing
        auto op_tok = lexer.get_tok();
        auto val = std::move(factor(lexer));
        return std::visit([&](const auto& val) -> val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<VT, float_type> || std::is_integral_v<VT>)
                return val;
            else
                throw make_parse_error(parse_error::num_rval_expected, op_tok); 
        }, val);
    }
    if (lexer.peeked_tok().id == token::sub) { // negation
        auto op_tok = lexer.get_tok();
        auto val = std::move(factor(lexer));
        return std::visit([&](const auto& val) -> val_type {
            using VT = std::decay_t<decltype(val)>;
            #pragma warning(disable : 4146) // suppress error negation of unsigned type
            if constexpr (std::is_same_v<VT, float_type> || std::is_integral_v<VT>)
                return static_cast<VT>(-val);
            else
                throw make_parse_error(parse_error::num_rval_expected, op_tok); 
        }, val);
    }
    if (lexer.peeked_tok().id == token::bnot) {
        auto op_tok = lexer.get_tok();
        auto val = std::move(factor(lexer));
        return std::visit([&](auto val) -> val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_integral_v<VT>)
                return static_cast<VT>(~val);
            else
                throw make_parse_error(parse_error::int_rval_expected, op_tok); 
        }, val);
    }

    auto val = std::move(primary_expression(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::pow) {
            auto op_tok = lexer.get_tok();
            auto factor_val = std::move(factor(lexer));
            validate_num_type(val, factor_val, op_tok);
            val = pow(get_as<float_type>(val), get_as<float_type>(factor_val));
        } else
            break;
    }
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::primary_expression(lookahead& lexer) -> val_type {
// <primary_expression> ::= ( <number> | <identifier> | <group> | <list> ) [ "!" | "!!" ]...
    val_type lval;
    if (lexer.peek_tok().id == token::number) {
        static_assert(!std::is_same_v<val_type, token::num_type>); // otherwise lval = casted(lexer.get_tok().num_val) would suffice
        auto tok_num = lexer.get_tok().num_val;
        if (std::holds_alternative<token::int_type>(tok_num))
            lval = std::get<token::int_type>(tok_num);
        else
            lval = std::get<token::float_type>(tok_num);
        lval = casted(lval);
    } else if (lexer.peeked_tok().id == token::identifier)
        lval = std::move(identifier(lexer));
    else if (lexer.peeked_tok().id == token::lparen)
        lval = std::move(group_or_list(lexer));
    else
        throw make_parse_error(parse_error::syntax_error, lexer.peeked_tok());

    for (;;) {
        if (lexer.peek_tok().id == token::fac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> val_type {
                using VT = std::decay_t<decltype(val)>;
                if constexpr (std::is_same_v<VT, float_type> || std::is_integral_v<VT>)
                    return tgamma(val + 1);
                else
                    throw make_parse_error(parse_error::num_lval_expected, op_tok);
            }, lval);
        } else if (lexer.peek_tok().id == token::dfac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> val_type {
                using VT = std::decay_t<decltype(val)>;
                if constexpr (std::is_same_v<VT, float_type> || std::is_integral_v<VT>) {
                    float_type cpi = cos(pi * val);
                    return pow(2.0, (1.0 + 2.0 * val - cpi) / 4.0)
                        * pow(pi, (cpi - 1.0) / 4.0) * tgamma(1.0 + val / 2.0);
                } else
                    throw make_parse_error(parse_error::num_lval_expected, op_tok);
            }, lval);
        } else
            break;
    }

    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::identifier(lookahead& lexer) -> val_type {
// <identifier> ::= <variable> | <unary_fn> | <list fn> | <internal value>
// <unary fn> ::= <unary fn name> <group>
// <list fn> ::= <list fn name> <list>
    lexer.get_expected_tok(token::identifier);

    // <variable>
    if (auto pos = vars_.find(lexer.cached_tok().tok_str); pos != vars_.end())
        return pos->second.num_val;

    // <unary fn name>
    for (auto pos = unary_fn_table.begin(); pos != unary_fn_table.end(); ++pos)
        if (identifiers_match(lexer.cached_tok().tok_str, pos->first))
            return pos->second(get_as<float_type>(group_or_list(lexer, 1)));

    // <list fn name>
    for (auto pos = list_fn_table.begin(); pos != list_fn_table.end(); ++pos)
        if (identifiers_match(lexer.cached_tok().tok_str, pos->first))
            return pos->second(group_or_list(lexer));

    // <internal value>
    if (identifiers_match(lexer.cached_tok().tok_str, "pi"))
        return pi;
    if (identifiers_match(lexer.cached_tok().tok_str, "e"))
        return e;
    if (identifiers_match(lexer.cached_tok().tok_str, "last"))
        return last_val_;

    throw make_parse_error(parse_error::undefined_identifier, lexer.cached_tok());
};

template <typename CharT>
auto calc_parser<CharT>::group_or_list(lookahead& lexer, size_t expected_count) -> val_type {
// <group_or_list> ::= '(' <expression> [ ',' <expression> ]... ')'
// expected_count: 0 is special value and means any count
    lexer.get_expected_tok(token::lparen);

    val_type val;
    list_type list;
    token tok;

    auto convert = [&](const auto& val) -> num_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            throw make_parse_error(parse_error::nested_list_invalid, tok);
        else
            return val;
    };

    for (;;) {
        val = std::move(expression(lexer));
        if (lexer.peek_tok().id != token::comma)
            break;
        tok = lexer.get_tok();
        list.emplace_back(std::visit(convert, val));
        if (expected_count && list.size() == expected_count)
            throw make_parse_error(parse_error::tok_expected, tok, token::rparen);
    }

    tok = lexer.get_expected_tok(token::rparen);
    if (expected_count && (list.size() + 1 != expected_count))
        throw make_parse_error(parse_error::num_val_expected, tok);

    if (!list.size())
        return val;
    list.emplace_back(std::visit(convert, val));
    return list;
}

template <typename CharT>
std::array<std::pair<const char*, typename calc_parser<CharT>::unary_fn>, 20> calc_parser<CharT>::unary_fn_table = {{
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
std::array<std::pair<const char*, typename calc_parser<CharT>::list_fn>, 6> calc_parser<CharT>::list_fn_table = {{
    {"sum", sum},
    {"prod", prod},
    {"avg", avg},
    {"variance", variance},
    {"stddev", stddev},
    {"median", median}
}};

template <typename CharT>
auto calc_parser<CharT>::sum(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return sum(val);
        else
            return val;
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::prod(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return prod(val);
        else
            return val;
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::avg(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return avg(val);
        else
            return val;
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::variance(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return variance(val);
        else
            return std::numeric_limits<float_type>::infinity();
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::stddev(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return stddev(val);
        else
            return std::numeric_limits<float_type>::infinity();
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::median(const val_type& num_val) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return median(val);
        else
            return val;
    }, num_val);
}

template <typename CharT>
auto calc_parser<CharT>::sum(const list_type& list) -> val_type {
    // setup val to be highest ranked alternative type in list
    size_t idx = 0;
    for (auto val : list)
        if (idx < val.index())
            idx = val.index();
    auto val = get_as(idx, 0);

    // accumulate sum into val
    for (auto list_val : list) {
        val = apply_promoted([&](auto val_, auto list_val_) -> val_type {
            return val_ + list_val_;
        }, val, list_val);
    }
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::prod(const list_type& list) -> val_type {
    // setup val as highest ranked alternative type in list
    size_t idx = 0;
    for (auto val : list)
        if (idx < val.index())
            idx = val.index();
    auto val = get_as(idx, 1);

    // accumulate product into val
    for (auto list_val : list) {
        val = apply_promoted([&](auto val_, auto list_val_) -> val_type {
            return val_ * list_val_;
        }, val, list_val);
    }

    return val;
}

template <typename CharT>
auto calc_parser<CharT>::avg(const list_type& list) -> val_type {
    float_type val = 0;
    for (auto list_val : list)
        val += get_as<float_type>(list_val);
    return val / list.size();
}

template <typename CharT>
auto calc_parser<CharT>::variance(const list_type& list) -> val_type {
    assert(list.size() > 0);
    float_type avg_val = get_as<float_type>(avg(list));
    float_type delta_sq_sum = 0;
    for (auto pval = list.begin(); pval != list.end(); ++pval) {
        auto d = get_as<float_type>(*pval) - avg_val;
        delta_sq_sum += d * d;
    }
    return delta_sq_sum / (list.size() - 1);
}

template <typename CharT>
auto calc_parser<CharT>::stddev(const list_type& list) -> val_type {
    return sqrt(get_as<float_type>(variance(list)));
}

template <typename CharT>
auto calc_parser<CharT>::median(const list_type& list) -> val_type {
    list_type list_ = list;
    std::sort(list_.begin(), list_.end());
    if (list.size() % 2)
        return get_as<float_type>(list_[list_.size() / 2]);
    return (get_as<float_type>(list[list.size() / 2 - 1]) +
        get_as<float_type>(list[list.size() / 2])) / 2.0;
}

#endif // CALC_PARSER_H
