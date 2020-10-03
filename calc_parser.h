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
            no_error, lexer_error, syntax_error, number_expected,
            undefined_identifier, tok_expected,
            num_operand_expected, num_operands_expected,
            int_operand_expected, int_operands_expected,
            negative_shift_invalid, division_by_0, unexpected_error,
            nested_list_invalid};
        static constexpr auto error_txt = std::array{
            // elements correspond with error_codes enums so enum can be used as index
            "no_error", "lexer error", "syntax error", "number expected",
            "is undefined", "was expected",
            "numeric operand expected", "numeric operands expected",
            "integer operand expected", "integer operands expected",
            "negative shift value is invalid", "division by 0", "unexpected error",
            "nested list at left is invalid"};
        error_codes error = no_error;
        token tok; // warning: has string_view that binds to the input string
        token_ids expected_tok = token::none; // valid for error == tok_expected
        void assert_view_is_valid_for(const CharT* input) const;
        auto error_str() const -> string;

        parse_error(error_codes error_, const token& tok_ = {}, token_ids expected_tok_ = token::unspecified) :
        // expected_tok is only valid for error == tok_expected
            error{error_}, tok{tok_}, expected_tok{expected_tok_} {}
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
        val_type val_var;
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
    static auto get_as(const val_type& val_var) -> T;
    // returns val_var with its value casted as T; precondition: T is one of
    // val_type's alternative types

    static auto get_as(const size_t& idx, const val_type& val_var) -> val_type;
    // returns val_var with it's value casted as the alternative type indicated
    // by idx (index)

    static auto promoted(const val_type& lval_var, const val_type& rval_var) -> std::pair<val_type, val_type>;
    // casts either lval_var's or rval_var's alternative to the higher promotion
    // level of the two and returns the modified value along with the other as a
    // pair

    template <typename Fn>
    static auto apply_promoted(const Fn& fn, const val_type& lval_var, const val_type& rval_var) -> auto;
    // fn: functor/lambda with operator()(T, T) -> val_type overloaded for each
    // of val_type's alternetive types (T). applies promoted() to lval_var and
    // rval_var, then applies fn() to lval_var's and rval_var's promoted values.
    // this function is used instead of std:visit to avoid generating
    // exponential number of functions, which can be done because promoted()
    // insures lval_var and rval_var will have the same alternative type

    auto casted(const val_type& val_var) const -> val_type;
    // if val_var is an integer type then this casts it to the type indicated by
    // int_result_tag_; else (for float_type) this simply returns val_var

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

    static auto sum(const val_type& val_var) -> val_type;
    static auto prod(const val_type& val_var) -> val_type;
    static auto avg(const val_type& val_var) -> val_type;
    static auto variance(const val_type& val_var) -> val_type;
    static auto stddev(const val_type& val_var) -> val_type;
    static auto median(const val_type& val_var) -> val_type;

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
//        var.val_var = casted(var.val_var);
}

template <typename CharT>
template <typename T>
auto calc_parser<CharT>::get_as(const val_type& val_var) -> T {
    // precondition: any of val_type's types is convertable to T
    switch (val_var.index()) {
    case 0: return static_cast<T>(*std::get_if<0>(&val_var));
    case 1: return static_cast<T>(*std::get_if<1>(&val_var));
    case 2: return static_cast<T>(*std::get_if<2>(&val_var));
    case 3: return static_cast<T>(*std::get_if<3>(&val_var));
    case 4: return static_cast<T>(*std::get_if<4>(&val_var));
    case 5: return static_cast<T>(*std::get_if<5>(&val_var));
    case 6: return static_cast<T>(*std::get_if<6>(&val_var));
    case 7: return static_cast<T>(*std::get_if<7>(&val_var));
    case 8: return static_cast<T>(*std::get_if<8>(&val_var));
    default:
        // list_type or missed one; list_type case should have been handled in
        // higher-level code
        assert(false);
        throw parse_error(parse_error::unexpected_error);
    }
}

template <typename CharT>
auto calc_parser<CharT>::get_as(const size_t& idx, const val_type& val_var) -> val_type {
    if (idx == val_var.index())
        return val_var;
    val_type val_var_;
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
        throw parse_error(parse_error::unexpected_error);
    }
    assert(idx == val_var_.index());
    return val_var_;
}

template <typename CharT>
auto calc_parser<CharT>::promoted(const val_type& lval_var, const val_type& rval_var) -> std::pair<val_type, val_type> {
    if (lval_var.index() < rval_var.index())
        return {get_as(rval_var.index(), lval_var), rval_var};
    if (lval_var.index() > rval_var.index())
        return {lval_var, get_as(lval_var.index(), rval_var)};
    return {lval_var, rval_var};
}

template <typename CharT>
template <typename Fn>
inline auto calc_parser<CharT>::apply_promoted(const Fn& fn, const val_type& lval_var, const val_type& rval_var) -> auto {
    auto [lval_var_, rval_var_] = std::move(promoted(lval_var, rval_var));
    assert(lval_var_.index() == rval_var_.index());
    switch (lval_var_.index()) {
    case 0: return fn(*std::get_if<0>(&lval_var_), *std::get_if<0>(&rval_var_));
    case 1: return fn(*std::get_if<1>(&lval_var_), *std::get_if<1>(&rval_var_));
    case 2: return fn(*std::get_if<2>(&lval_var_), *std::get_if<2>(&rval_var_));
    case 3: return fn(*std::get_if<3>(&lval_var_), *std::get_if<3>(&rval_var_));
    case 4: return fn(*std::get_if<4>(&lval_var_), *std::get_if<4>(&rval_var_));
    case 5: return fn(*std::get_if<5>(&lval_var_), *std::get_if<5>(&rval_var_));
    case 6: return fn(*std::get_if<6>(&lval_var_), *std::get_if<6>(&rval_var_));
    case 7: return fn(*std::get_if<7>(&lval_var_), *std::get_if<7>(&rval_var_));
    case 8: return fn(*std::get_if<8>(&lval_var_), *std::get_if<8>(&rval_var_));
    case 9: return fn(*std::get_if<9>(&lval_var_), *std::get_if<9>(&rval_var_));
    default: // missed one
        assert(false);
        throw parse_error(parse_error::unexpected_error);
    }
}

template <typename CharT>
auto calc_parser<CharT>::casted(const val_type& val_var) const -> val_type {
    return std::visit([&](const auto& val) -> val_type {
        using VT = std::decay_t<decltype(val)>;
        if constexpr (std::is_integral_v<VT>)
            return get_as(int_result_tag_, val_var);
        else if constexpr (std::is_floating_point_v<VT> || std::is_same_v<VT, list_type>)
            return val;
            // TODO: for list_type, not sure if casted should be applied to
            // elements of list; not doing so for now
        else { // missed one
            static_assert(false);
            throw parse_error(parse_error::unexpected_error);
        }
    }, val_var);
}

template <typename CharT>
inline bool calc_parser<CharT>::identifiers_match(string_view inp_symb, const char* tab_symb) {
    return char_helper::eq(inp_symb, tab_symb);
    // may want to support case insensitive match in the future--maybe
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
        throw parse_error(parse_error::syntax_error, lexer.cached_tok());
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
auto calc_parser<CharT>::arithmetic_expr(lookahead& lexer) -> val_type {
// <arithmetic expr> ::= <bor term> [ "|" <bor term> ]...
    auto lval_num = std::move(bor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bor) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(bor_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
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
auto calc_parser<CharT>::bor_term(lookahead& lexer) -> val_type {
// <bor term> ::= <bxor term> [ "^" <bxor term> ]...
    auto lval_num = std::move(bxor_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::bxor) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(bxor_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
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
auto calc_parser<CharT>::bxor_term(lookahead& lexer) -> val_type {
// <bxor term> ::= <band term> [ "&" <band term> ]...
    auto lval_num = std::move(band_term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::band) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(band_term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
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
auto calc_parser<CharT>::band_term(lookahead& lexer) -> val_type {
// <band term> ::= <shift term> [ ( "<<" | "<<<" | ">>" | ">>>" ) <shift term> ]...
    token op_tok;

    auto shift_arg_in_range = [&](const auto& lval, const auto& shift_arg) -> bool {
        // assume shift_arg is valid only if positive and less than # bits in
        // lval's type. if shift_arg is negative then it's unusable; exception
        // will be thrown in that case. if shift_arg is >= # bits in lval's type
        // then we will simulate shifting beyond that limit.
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
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> val_type {
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
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> val_type {
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
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> val_type {
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
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> val_type {
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
auto calc_parser<CharT>::shift_term(lookahead& lexer) -> val_type {
// <shift term> ::= <term> [ ( "+" | "-" ) <term> ]...
    auto lval_num = std::move(term(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::add) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_floating_point_v<LVT> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval + rval);
                else
                    throw parse_error(parse_error::num_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::sub) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(term(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_floating_point_v<LVT> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval - rval);
                else
                    throw parse_error(parse_error::num_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else {
            break;
        }
    }
    return lval_num;
}

template <typename CharT>
auto calc_parser<CharT>::term(lookahead& lexer) -> val_type {
// <term> ::= <factor> [ ( "*" | "/" | "%" ) <factor> ]...
    auto lval_num = std::move(factor(lexer));
    for (;;) {
        if (lexer.peek_tok().id == token::mul) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_floating_point_v<LVT> || std::is_integral_v<LVT>)
                    return static_cast<LVT>(lval * rval);
                else
                    throw parse_error(parse_error::num_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peeked_tok().id == token::div) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                static_assert(std::is_same_v<LVT, RVT>);
                if constexpr (std::is_floating_point_v<LVT>)
                    // for floating point, let division by 0 resolve to inf
                    return static_cast<LVT>(lval / rval);
                else if constexpr (std::is_integral_v<LVT>) {
                    if (rval == 0)
                        throw parse_error(parse_error::division_by_0, op_tok);
                    return static_cast<LVT>(lval / rval);
                }
                else
                    throw parse_error(parse_error::num_operands_expected, op_tok);
            }, lval_num, rval_num);
        } else if (lexer.peek_tok().id == token::mod) {
            auto op_tok = lexer.get_tok();
            auto rval_num = std::move(factor(lexer));
            lval_num = apply_promoted([&](const auto& lval, const auto& rval) -> val_type {
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
        } else
            break;
    }
    return lval_num;
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
        auto val_num = std::move(factor(lexer));
        return std::visit([&](const auto& val) -> val_type {
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
        return std::visit([&](const auto& val) -> val_type {
            using VT = std::decay_t<decltype(val)>;
            if constexpr (std::is_floating_point_v<VT> || std::is_signed_v<VT>)
                return static_cast<VT>(-val);
            else if constexpr (std::is_unsigned_v<VT>) // (note: MSVC++ doesn't like negating unsigned type)
                return val;
            else
                throw parse_error(parse_error::num_operand_expected, op_tok); 
        }, val_num);
    }
    if (lexer.peeked_tok().id == token::bnot) {
        auto op_tok = lexer.get_tok();
        auto val_num = std::move(factor(lexer));
        return std::visit([&](auto val) -> val_type {
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
            lval_num = std::visit([&](const auto& lval, const auto& rval) -> val_type {
                using LVT = std::decay_t<decltype(lval)>;
                using RVT = std::decay_t<decltype(rval)>;
                if constexpr ((std::is_floating_point_v<LVT> || std::is_integral_v<LVT>)
                        && (std::is_floating_point_v<RVT> || std::is_integral_v<RVT>))
                    return pow(lval, rval);
                else
                    throw parse_error(parse_error::num_operands_expected, op_tok); 
            }, lval_num, rval_num);
        } else
            break;
    }
    return lval_num;
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
        throw parse_error(parse_error::syntax_error, lexer.peeked_tok());

    for (;;) {
        if (lexer.peek_tok().id == token::fac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> val_type {
                using VT = std::decay_t<decltype(val)>;
                if constexpr (std::is_floating_point_v<VT> || std::is_integral_v<VT>)
                    return tgamma(val + 1);
                else
                    throw parse_error(parse_error::num_operand_expected, op_tok);
            }, lval);
        } else if (lexer.peek_tok().id == token::dfac) {
            auto op_tok = lexer.get_tok();
            lval = std::visit([&](const auto& val) -> val_type {
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
auto calc_parser<CharT>::identifier(lookahead& lexer) -> val_type {
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

    throw parse_error(parse_error::undefined_identifier, lexer.cached_tok());
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
            throw parse_error(parse_error::nested_list_invalid, tok);
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
            throw parse_error(parse_error::tok_expected, tok, token::rparen);
    }

    tok = lexer.get_expected_tok(token::rparen);
    if (expected_count && (list.size() + 1 != expected_count))
        throw parse_error(parse_error::number_expected, tok);

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
auto calc_parser<CharT>::sum(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return sum(val);
        else
            return val;
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::prod(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return prod(val);
        else
            return val;
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::avg(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return avg(val);
        else
            return val;
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::variance(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return variance(val);
        else
            return std::numeric_limits<float_type>::infinity();
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::stddev(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return stddev(val);
        else
            return std::numeric_limits<float_type>::infinity();
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::median(const val_type& val_var) -> val_type {
    return std::visit([](const auto& val) -> val_type {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, list_type>)
            return median(val);
        else
            return val;
    }, val_var);
}

template <typename CharT>
auto calc_parser<CharT>::sum(const list_type& list) -> val_type {
    float_type val = 0;
    for (auto list_val : list)
        val += get_as<float_type>(list_val);
    return val;
}

template <typename CharT>
auto calc_parser<CharT>::prod(const list_type& list) -> val_type {
    float_type val = 1;
    for (auto list_val : list)
        val *= get_as<float_type>(list_val);
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
    list_type list_;
    list_.reserve(list.size());
    for (auto& val: list)
        list_.emplace_back(get_as<float_type>(val));
    std::sort(list_.begin(), list_.end());

    if (list_.size() % 2)
        return get_as<float_type>(list_[list_.size() / 2]);
    return (get_as<float_type>(list_[list_.size() / 2 - 1]) +
        get_as<float_type>(list_[list_.size() / 2])) / 2.0;
}

#endif // CALC_PARSER_H
