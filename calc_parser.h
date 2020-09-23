#pragma once
#ifndef CALC_PARSER_H
#define CALC_PARSER_H

#include "calc_lexer.h"
#include <map>
#include <set>
#include <type_traits>
#include <functional>

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

    calc_parser() : notify_vars_changed{[]{}} {} // init notify_vars_changed with do-nothing lambda
    
    using notify_vars_changed_fn = std::function<void()>;
    calc_parser(const notify_vars_changed_fn& notify_vars_changed_)
        : notify_vars_changed{notify_vars_changed_} {}

    calc_parser(const calc_parser&) = delete;
    calc_parser& operator=(calc_parser&) = delete;
    // disable copy/assignment; don't think can trivially copy vars_ below
    // because of how its elements bind to var_keys

    using num_type = std::variant<
        std::int8_t, std::uint8_t, std::int16_t, std::uint16_t, std::int32_t,
        std::uint32_t, std::int64_t, std::uint64_t, float_type>;
        // in order of ascending promotability
    static constexpr auto num_type_short_txt = std::array
        // short text for UI.
        // elements correspond with num_type variant so index() can be used as index
        {"Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Float"};
    using widest_uint_type = std::uint64_t;
    using widest_int_type = std::int64_t;

    enum int_result_tags : unsigned {
        int8_tag, uint8_tag, int16_tag, uint16_tag, int32_tag, uint32_tag, int64_tag, uint64_tag};
        // must correspond with num_type's alternative types
    auto int_result_tag() const -> auto {return int_result_tag_;}
    void int_result_tag(int_result_tags int_result_tag);

    struct parse_error { // exception
        enum error_codes {no_error, lexer_error, syntax_error,
            undefined_identifier, tok_expected, int_val_expected,
            int_lval_expected, int_rval_expected, rval_negative_invalid,
            division_by_0};
        static constexpr auto error_txt = std::array{
            // elements correspond with error_codes enums so enum can be used as index
            "no_error", "lexer error", "syntax error",
            "is undefined", "was expected", "integer expected for operand (check mode)",
            "integer expected for left operand (check mode)", "integer expected for right operand (check mode)", "negative right operand invalid",
            "division by 0"};
        error_codes error = no_error;
        token tok; // warning: string_views will be relative to and only valid for the input string
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

    auto last_val() const -> num_type;
    // returns last evaluated result

    using var_keys_type = std::multiset<string>;
    struct variable {
        const typename var_keys_type::iterator key_pos; // see var_keys below
        num_type num_val;
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
        lookahead(const CharT* input, radices default_radix) :
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

    num_type last_val_ = 0.0;
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
    // containers; this eliminates vectors despite the appeal of their
    // simplicity

    template <typename T>
    static auto get_as(num_type num_val) -> T;
    // returns num_val with its value casted as T; precondition: T is one of
    // num_type's alternative types

    static auto get_as(size_t idx, num_type num_val) -> num_type;
    // returns num_val with it's value casted as the alternative type indicated
    // by idx (index)

    auto casted(num_type num_val) const -> num_type;
    // if num_val is an integer type then this casts it to the type indicated by
    // int_result_tag_; else (for float_type) this simply returns num_val

    static auto promoted(num_type lnum_val, num_type rnum_val) -> std::pair<num_type, num_type>;
    // casts either lnum_val's or rnum_val's alternative to the higher promotion
    // level of the two and returns the modified value along with the other as a
    // pair

    template <typename Fn>
    static auto apply_promoted(Fn fn, num_type lnum_val, num_type rnum_val) -> auto;
    // fn: functor/lambda with operator()(T, T) -> num_type overloaded for each
    // of num_type's alternetive types (T). applies promoted() to lnum_val and
    // rnum_val, then applies fn() to lnum_val's and rnum_val's promoted values.
    // this function is used instead of std:visit to avoid generating 9x9
    // operator() overloads, which can be done because promoted() insures
    // lnum_val and rnum_val will have the same alternative type, thus only 9
    // overloads are needed.

    static void validate_int_only(num_type rval, num_type lval, const token& op_tok);
    
    static auto make_parse_error(error_codes error, const token& tok, token_ids expected_tok_id = token::unspecified) -> parse_error;
    // expected_tok is only valid for error == parse_error::tok_expected

    auto expression(lookahead& lexer) -> num_type;
    auto arithmetic_expr(lookahead& lexer)-> num_type;
    auto bor_term(lookahead& lexer) -> num_type;
    auto bxor_term(lookahead& lexer) -> num_type;
    auto band_term(lookahead& lexer) -> num_type;
    auto shift_term(lookahead& lexer) -> num_type;
    auto term(lookahead& lexer)-> num_type;
    auto factor(lookahead& lexer)-> num_type;
    auto primary_expression(lookahead& lexer)-> num_type;
    auto identifier(lookahead& lexer)-> num_type;
    auto group(lookahead& lexer)-> num_type;

    using unary_fn = float_type (*)(float_type);
    static std::array<std::pair<const char*, unary_fn>, 20> unary_fn_table;
    // unary_fn_table: simple unordered array; should be small enough that
    // simple linear search will be adequate

    static bool identifiers_match(string_view inp_symb, const char* tab_symb);
    // input identifier matches identifier in table?
    // internally, identifier in table is const char* regardless of CharT

    static auto fac(num_type num_val) -> num_type; // factorial; generalized for fractional numbers
    static auto dfac(num_type num_val) -> num_type; // double factorial

    static constexpr auto pi = 3.14159265358979323846;
    static constexpr auto e = 2.71828182845904523536;

    notify_vars_changed_fn notify_vars_changed;
};

#include "parse_error.h"
#include "lookahead.h"

template <typename CharT>
inline auto calc_parser<CharT>::last_val() const -> num_type {
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
auto calc_parser<CharT>::get_as(num_type num_val) -> T {
    // precondition: any of num_type's types is convertable to T
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
    default: assert(false); return static_cast<T>(0); // missed one; return arbitrary value
    }
}

template <typename CharT>
auto calc_parser<CharT>::get_as(size_t idx, num_type num_val) -> num_type {
    if (idx == num_val.index())
        return num_val;
    switch (idx) {
    case 0: num_val = get_as<std::int8_t>(num_val); break;
    case 1: num_val = get_as<std::uint8_t>(num_val); break;
    case 2: num_val = get_as<std::int16_t>(num_val); break;
    case 3: num_val = get_as<std::uint16_t>(num_val); break;
    case 4: num_val = get_as<std::int32_t>(num_val); break;
    case 5: num_val = get_as<std::uint32_t>(num_val); break;
    case 6: num_val = get_as<std::int64_t>(num_val); break;
    case 7: num_val = get_as<std::uint64_t>(num_val); break;
    case 8: num_val = get_as<float_type>(num_val); break;
    default: assert(false); return 0.0; // missed one; return arbitrary value
    }
    assert(idx == num_val.index());
    return num_val;
}

template <typename CharT>
auto calc_parser<CharT>::casted(num_type num_val) const -> num_type {
    if (std::get_if<float_type>(&num_val))
        return {num_val};
    return {get_as(int_result_tag_, num_val)};
}

template <typename CharT>
auto calc_parser<CharT>::promoted(num_type lnum_val, num_type rnum_val) -> std::pair<num_type, num_type> {
    if (lnum_val.index() == rnum_val.index()) // this should be the typical case
        return {lnum_val, rnum_val};
    if (lnum_val.index() < rnum_val.index())
        return {get_as(rnum_val.index(), lnum_val), rnum_val};
    if (lnum_val.index() > rnum_val.index())
        return {lnum_val, get_as(lnum_val.index(), rnum_val)};
    return {lnum_val, rnum_val};
}

template <typename CharT>
template <typename Fn>
auto calc_parser<CharT>::apply_promoted(Fn fn, num_type lnum_val, num_type rnum_val) -> auto {
    std::tie(lnum_val, rnum_val) = promoted(lnum_val, rnum_val);
    assert(lnum_val.index() == rnum_val.index());
    switch (lnum_val.index()) {
    case 0: return fn(*std::get_if<0>(&lnum_val), *std::get_if<0>(&rnum_val));
    case 1: return fn(*std::get_if<1>(&lnum_val), *std::get_if<1>(&rnum_val));
    case 2: return fn(*std::get_if<2>(&lnum_val), *std::get_if<2>(&rnum_val));
    case 3: return fn(*std::get_if<3>(&lnum_val), *std::get_if<3>(&rnum_val));
    case 4: return fn(*std::get_if<4>(&lnum_val), *std::get_if<4>(&rnum_val));
    case 5: return fn(*std::get_if<5>(&lnum_val), *std::get_if<5>(&rnum_val));
    case 6: return fn(*std::get_if<6>(&lnum_val), *std::get_if<6>(&rnum_val));
    case 7: return fn(*std::get_if<7>(&lnum_val), *std::get_if<7>(&rnum_val));
    case 8: return fn(*std::get_if<8>(&lnum_val), *std::get_if<8>(&rnum_val));
    default: assert(false); return decltype(fn(*std::get_if<8>(&lnum_val), *std::get_if<8>(&rnum_val)))(0); // missed one; return arbitrary value
    }
}

template <typename CharT>
inline auto calc_parser<CharT>::make_parse_error(error_codes error, const token& tok, token_ids expected_tok_id) -> parse_error {
    return parse_error{error, tok, expected_tok_id};
}

template <typename CharT>
bool calc_parser<CharT>::identifiers_match(string_view inp_symb, const char* tab_symb) {
    if (inp_symb.size() != strlen(tab_symb))
        return false;
    auto inp_pos = inp_symb.begin();
    auto tab_pos = tab_symb;
    for (; inp_pos != inp_symb.end(); ++inp_pos, ++tab_pos)
        if (*inp_pos != *tab_pos)
            return false;
    return true;
    // may want to support case insensitive match in the future--maybe
}

template <typename CharT>
inline void calc_parser<CharT>::validate_int_only(num_type lnum_val, num_type rnum_val, const token& op_tok) {
    std::visit([&](auto lval) {
        if constexpr (!std::is_integral_v<decltype(lval)>)
            throw make_parse_error(parse_error::int_lval_expected, op_tok);
    }, lnum_val);
    std::visit([&](auto rval) {
        if constexpr (!std::is_integral_v<decltype(rval)>)
            throw make_parse_error(parse_error::int_rval_expected, op_tok);
    }, rnum_val);
    // lnum_val and rnum_val are visied independently to avoid generating 9x9
    // functions
}

template <typename CharT>
auto calc_parser<CharT>::eval(const CharT* input) -> bool {
    lookahead lexer = {input, default_radix_};
    if (lexer.peek_tok().id == token::end)
        return false;
    last_val_ = expression(lexer);
    if (lexer.get_tok().id != token::end)
        throw make_parse_error(parse_error::syntax_error, lexer.cached_tok());
    return true;
}

template <typename CharT>
auto calc_parser<CharT>::expression(lookahead& lexer) -> num_type {
// <expression> ::= <identifier> "=" ( <expression> | <end> ) | <arithmetic expr>
    num_type val;
    lexer.peek_tok2();
    if (lexer.peeked_tok().id == token::identifier && lexer.peeked_tok2().id == token::eq) {
        auto key = lexer.get_tok().tok_str;
        lexer.get_tok(); // eat "="
        auto pos = vars_.find(key);
        if (lexer.peek_tok().id == token::end) {
        // erase the variable (if it's not already so)
            if (pos != vars_.end()) {
                var_keys.erase(pos->second.key_pos); // erase corresponding entry in var_keys
                vars_.erase(pos);
            }
            val = std::numeric_limits<float_type>::quiet_NaN();
        } else if (pos == vars_.end()) {
        // variable not found; insert new one with <expression>
            val = expression(lexer);
            auto var_key_pos = var_keys.emplace(string{key.begin(), key.end()});
            vars_.try_emplace(*var_key_pos, variable{var_key_pos, val});
        } else
        // variable found; assign to it <expression>
            val = pos->second.num_val = expression(lexer);
        notify_vars_changed();
    } else
        val = arithmetic_expr(lexer);
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::arithmetic_expr(lookahead& lexer) -> num_type {
// <arithmetic expr> ::= <bor term> [ "|" <bor term> ]...
    auto lval = bor_term(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::bor) {
            auto& op_tok = lexer.get_tok();
            auto rval = bor_term(lexer);
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                if constexpr (std::is_integral_v<decltype(lval)>)
                    return static_cast<decltype(lval)>(lval | rval);
                else { // should never be invoked
                    assert(false);
                    return 0.0; // arbitrary value
                }
            }, lval, rval);
        } else
            break;
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::bor_term(lookahead& lexer) -> num_type {
// <bor term> ::= <bxor term> [ "^" <bxor term> ]...
    auto lval = bxor_term(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::bxor) {
            auto& op_tok = lexer.get_tok();
            auto rval = bxor_term(lexer);
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                if constexpr (std::is_integral_v<decltype(lval)>)
                    return static_cast<decltype(lval)>(lval ^ rval);
                else { // should never be invoked
                    assert(false);
                    return 0.0; // arbitrary value
                }
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::bxor_term(lookahead& lexer) -> num_type {
// <bxor term> ::= <band term> [ "&" <band term> ]...
    auto lval = band_term(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::band) {
            auto& op_tok = lexer.get_tok();
            auto rval = band_term(lexer);
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                if constexpr (std::is_integral_v<decltype(lval)>)
                    return static_cast<decltype(lval)>(lval & rval);
                else { // should never be invoked
                    assert(false);
                    return 0.0; // arbitrary value
                }
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::band_term(lookahead& lexer) -> num_type {
// <band term> ::= <shift term> [ ( "<<" | "<<<" | ">>" | ">>>" ) <shift term> ]...
    token op_tok;

    // all this complexity is to try to insure consistent well-defined behavior.
    // for a << b or a >> b, the only behavior that's well-defined in standard
    // C++ is where a and b are positive/unsigned and b < number of bits in a.

    auto shift_arg_in_range = [&](auto lval, auto shift_arg) -> bool {
        // precondition: lval and shift_arg are integral (integer) types
        static_assert(std::is_integral_v<decltype(shift_arg)>);
        if constexpr (std::is_signed_v<decltype(shift_arg)>)
            if (shift_arg < 0)
                throw make_parse_error(parse_error::rval_negative_invalid, op_tok);
        return shift_arg < (sizeof(decltype(lval)) * CHAR_BIT); // assume shift_arg ok only if less than # bits in lval's type
    };

    auto lshiftl_op = [](auto lval, auto rval) -> auto { // logical shift left
        return static_cast<decltype(lval)>(std::make_unsigned_t<decltype(lval)>(lval) << rval);
    };
    auto lshiftr_op = [](auto lval, auto rval) -> auto { // logical shift right
        return static_cast<decltype(lval)>(std::make_unsigned_t<decltype(lval)>(lval) >> rval);
    };
    auto bnot_op = [](auto val) -> auto { // bitwise negation cast back as original type (avoid promotion of, e.g., 8 bit type to int)
        return static_cast<decltype(val)>(~val);
    };

    auto logical_shift = [&](auto lval, auto rval, auto shift_op) -> num_type { // arithmetic and logical shift are the same in most cases
        if constexpr (std::is_integral_v<decltype(lval)> && std::is_integral_v<decltype(rval)>) {
            if (!shift_arg_in_range(lval, rval))
                return static_cast<decltype(lval)>(0);
            return shift_op(lval, rval);
        } else {
            assert(false); // should never be invoked
            return 0.0; // arbitrary value
        }
    };

    auto lshiftl = [&](auto lval, auto rval) -> num_type { // logical shift left
        return logical_shift(lval, rval, lshiftl_op);
    };

    auto lshiftr = [&](auto lval, auto rval) -> num_type { // logical shift right
        return logical_shift(lval, rval, lshiftr_op);
    };

    auto ashiftl = [&](auto lval, auto rval) -> num_type { // arithmetic shift left; equivalent to logical shift left
        return logical_shift(lval, rval, lshiftl_op);
    };

    auto ashiftr = [&](auto lval, auto rval) -> num_type { // arithmetic shift right
        if constexpr (std::is_integral_v<decltype(lval)> && std::is_integral_v<decltype(rval)>) {
            if constexpr (std::is_signed_v<decltype(lval)>)
                if (lval < 0) {
                    if (!shift_arg_in_range(lval, rval))
                        return static_cast<decltype(lval)>(~static_cast<decltype(lval)>(0));
                    auto fill_bits =bnot_op(lshiftr_op(bnot_op(static_cast<decltype(lval)>(0)), rval));
                    return static_cast<decltype(lval)>(lshiftr_op(lval, rval) | fill_bits);
                }
            assert(lval >= 0);
            if (!shift_arg_in_range(lval, rval))
                return static_cast<decltype(lval)>(0);
            return lshiftr_op(lval, rval);
        } else {
            assert(false); // should never be invoked
            return 0.0; // arbitrary value
        }
    };

    auto lval = shift_term(lexer);
    auto do_shift = [&](auto shift_fn) {
        auto& op_tok = lexer.get_tok();
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
auto calc_parser<CharT>::shift_term(lookahead& lexer) -> num_type {
// <shift term> ::= <term> [ ( "+" | "-" ) <term> ]...
    auto lval = term(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::add) {
            lexer.get_tok();
            auto rval = term(lexer);
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                return static_cast<decltype(lval)>(lval + rval);
            }, lval, rval);
        } else if (lexer.peeked_tok().id == token::sub) {
            lexer.get_tok();
            auto rval = term(lexer);
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                return static_cast<decltype(lval)>(lval - rval);
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::term(lookahead& lexer) -> num_type {
// <term> ::= <factor> [ ( "*" | "/" | "%" ) <factor> ]...
    auto lval = factor(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::mul) {
            lexer.get_tok();
            auto rval = factor(lexer);
            lval = apply_promoted([](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                return static_cast<decltype(lval)>(lval * rval);
            }, lval, rval);
        } else if (lexer.peeked_tok().id == token::div) {
            auto& op_tok = lexer.get_tok();
            auto rval = factor(lexer);
            lval = apply_promoted([&](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                if constexpr (std::is_integral_v<decltype(lval)>)
                    if (rval == 0)
                        throw make_parse_error(parse_error::division_by_0, op_tok);
                // for floating point, let division by 0 resolve to inf
                return static_cast<decltype(lval)>(lval / rval);
            }, lval, rval);
        } else if (lexer.peek_tok().id == token::mod) {
            auto& op_tok = lexer.get_tok();
            auto rval = factor(lexer);
            validate_int_only(lval, rval, op_tok); // do before apply_promoted() possibly changes integer type to float_type
            lval = apply_promoted([&](auto lval, auto rval) -> num_type {
                static_assert(std::is_same_v<decltype(lval), decltype(rval)>);
                if constexpr (std::is_integral_v<decltype(lval)>) {
                    if (rval == 0)
                        throw make_parse_error(parse_error::division_by_0, op_tok);
                    return static_cast<decltype(lval)>(lval % rval);
                } else { // should never be invoked
                    assert(false);
                    return 0.0; // arbitrary value
                }
            }, lval, rval);
        } else {
            break;
        }
    }
    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::factor(lookahead& lexer) -> num_type {
// <factor> ::= <unary expression> | <exponentiation>
// <unary expression> ::= ( "+" | "-" | "~" ) <factor>
// <exponentiation> ::= <primary expression> [ "**" <factor> ]...
// exponentiation is right-associative.
// exponentiation has higher precedence than unary negation (+/-).
// bitwise-not should probably have higher precedence than exponentiation but
// that would result in ~-1 being a syntax error, so it's here.
    if (lexer.peek_tok().id == token::add) { // unary +, basically do nothing
        lexer.get_tok();
        return factor(lexer);
    }
    if (lexer.peeked_tok().id == token::sub) { // negation
        lexer.get_tok();
        auto val = factor(lexer);
        return std::visit([](auto val) -> num_type {
            #pragma warning(disable : 4146) // suppress error negation of unsigned type
            return static_cast<decltype(val)>(-val);
        }, val);
    }
    if (lexer.peeked_tok().id == token::bnot) {
        auto& op_tok = lexer.get_tok();
        auto val = factor(lexer);
        return std::visit([&](auto val) -> num_type {
            if constexpr (std::is_same_v<decltype(val), float_type>)
                throw make_parse_error(parse_error::int_val_expected, op_tok); 
            else {
                static_assert(std::is_integral_v<decltype(val)>);
                return static_cast<decltype(val)>(~val);
            }
        }, val);
    }

    auto val = primary_expression(lexer);
    for (;;) {
        if (lexer.peek_tok().id == token::pow) {
            lexer.get_tok();
            val = pow(get_as<float_type>(val), get_as<float_type>(factor(lexer)));
        } else {
            break;
        }
    }
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::primary_expression(lookahead& lexer) -> num_type {
// <primary_expression> ::= ( <number> | <identifier> | <group> ) [ "!" | "!!" ]...
    num_type lval;
    if (lexer.peek_tok().id == token::number) {
        static_assert(!std::is_same_v<num_type, token::num_type>); // otherwise lval = casted(lexer.get_tok().num_val) would suffice
        auto tok_num = lexer.get_tok().num_val;
        if (auto p = std::get_if<token::int_type>(&tok_num))
            lval = get_as<token::int_type>(*p);
        else
            lval = std::get<token::float_type>(tok_num);
        lval = casted(lval);
    } else if (lexer.peeked_tok().id == token::identifier)
        lval = identifier(lexer);
    else if (lexer.peeked_tok().id == token::lparen)
        lval = group(lexer);
    else
        throw make_parse_error(parse_error::syntax_error, lexer.peeked_tok());

    for (;;) {
        if (lexer.peek_tok().id == token::fac) {
            lexer.get_tok();
            lval = fac(lval);
        } else if (lexer.peek_tok().id == token::dfac) {
            lexer.get_tok();
            lval = dfac(lval);
        } else {
            break;
        }
    }

    return lval;
}

template <typename CharT>
auto calc_parser<CharT>::identifier(lookahead& lexer) -> num_type {
// <identifier> ::= <variable> | <function> | <internal value>
// <function> ::= <unary fn name> <group>
    lexer.get_expected_tok(token::identifier);

    // <variable>
    if (auto pos = vars_.find(lexer.cached_tok().tok_str); pos != vars_.end())
        return pos->second.num_val;

    // <unary fn name>
    for (auto pos = unary_fn_table.begin(); pos != unary_fn_table.end(); ++pos)
        if (identifiers_match(lexer.cached_tok().tok_str, pos->first))
            return pos->second(get_as<float_type>(group(lexer)));

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
auto calc_parser<CharT>::group(lookahead& lexer) -> num_type {
// <group> ::= "(" <expression> ")"
    lexer.get_expected_tok(token::lparen);
    auto val = expression(lexer);
    lexer.get_expected_tok(token::rparen);
    return val;
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
auto calc_parser<CharT>::fac(num_type num_val) -> num_type {
    return tgamma(get_as<float_type>(num_val) + 1);
}

template <typename CharT>
auto calc_parser<CharT>::dfac(num_type num_val) -> num_type { // double factorial
    auto n = get_as<float_type>(num_val);
    // formula comes from wolfram mathworld
    float_type cpi = cos(pi * n);
    return pow(2.0, (1.0 + 2.0 * n - cpi) / 4.0)
        * pow(pi, (cpi - 1.0) / 4.0) * tgamma(1.0 + n / 2.0);
}

#endif // CALC_PARSER_H
