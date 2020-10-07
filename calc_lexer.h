#pragma once
#ifndef CALC_LEXER_H
#define CALC_LEXER_H

#include <variant>
#include <array>
#include <cassert>
#include <stdexcept>
#include <string>
#include <string_view>
#include <variant>
#include <climits>
#include "char_helper.h"

// calc_lexer is the tokenizer (lexical analyzer) for the parser calc_parser

using namespace std::literals;

template <typename CharT>  // CharT: char or wchar_t
struct token {
// contains information about a token scanned by calc_lexer; created by
// calc_lexer and used by calc_parser
    using string_view = std::basic_string_view<CharT>;

    enum token_ids {unspecified, end, number, identifier, add, sub, mul, div,
        mod, pow, fac, dfac, lparen, rparen, eq, ashiftl, ashiftr, lshiftl,
        lshiftr, band, bor, bxor, bnot, comma};
    static constexpr auto token_txt = std::array
        // text suitable for parser error message.
        // elements correspond with token_ids enums so enum can be used as index
        {"unspecified", "end", "number", "identifier", "\"+\"", "\"-\"", "\"*\"", "\"/\"",
        "\"%\"", "\"**\"","\"!\"", "\"!!\"", "\"(\"", "\")\"", "\"=\"", "\"<<\"", "\">>\"", "\"<<<\"",
        "\">>>\"", "\"&\"", "\"|\"", "\"^\"", "\"~\"", "\",\""};
    token_ids id = unspecified;

    using int_type = std::uint64_t;
    // calc_lexer will scan integers as unsigned type. this is to allow for
    // literals such as 0xffffffffffffffff, but also because '-' is treated as
    // it's own token and not part of literal value. consequently,
    // 18446744073709551615 is a valid integer here and is equivalent to -1.

    using float_type = double;
    using num_type = std::variant<float_type, int_type>;
    num_type num_val = float_type();

    enum radices : unsigned char {decimal = 0, base2 = 2, base8 = 8, base10 = 10, base16 = 16};

    string_view tok_str = {}; // view of scanned token in input string, or default constructed view

    enum error_codes {no_error, integer_expected, out_of_range, invalid_number, mfac_unsupported};
    static constexpr auto error_txt = std::array{
        // elements correspond with error_codes enums so enum can be used as index
        "no_error", "must be an integer", "is out of range", "is an invalid number", "- multifactorial is unsupported"};
    error_codes error = no_error;

    token(token_ids id_, num_type num_val_, string_view tok_str_, error_codes error_) :
            id{id_}, num_val{num_val_}, tok_str{tok_str_}, error{error_}  {}

    token() = default;
};

template <typename CharT> // CharT: char or wchar_t
class calc_lexer {
public:
    using token = token<CharT>;
    using string_view = typename token::string_view;
    using string = std::basic_string<CharT>;
    using float_type = typename token::float_type;
    using int_type = typename token::int_type;
    using num_type = typename token::num_type;
    using radices = typename token::radices;
    using token_ids = typename token::token_ids;
    using error_codes = typename token::error_codes;

    radices default_radix = decimal;

    calc_lexer(const CharT* input, radices default_radix);
    // input: assumed to point to a null-terminated (c-style) string. if input
    // is null then throws std::invalid_argument. stores the pointer (and not a
    // copy of the string), thus has pointer invalidation rule. string_view was
    // considered for input's type but c-style string was chosen for performance
    // and implementation considerations.
    // default_radix: default radix (number base) for numbers.

    calc_lexer() = default;

    auto get_tok() -> token;

private:
    const CharT* in_begin = 0;
    const CharT* in_pos = 0; // current position in input
    string num_buf;
    auto make_token(token_ids id, const CharT* tok_begin, const CharT* tok_end, error_codes error, num_type num_val = float_type(0)) -> token;

    struct scan_result {
        num_type num_val;
        error_codes error = error_codes::no_error;
    };
    auto scan_as_number() noexcept -> scan_result;
    static auto scan_helper_int(const CharT* in_pos, unsigned radix) noexcept -> scan_result;
    static auto scan_helper_float(const CharT* in_pos) noexcept -> scan_result;
};

template <typename CharT>
inline calc_lexer<CharT>::calc_lexer(const CharT* input, radices default_radix_) :
    in_begin{input}, in_pos{input}, default_radix{default_radix_}
{
    assert(input);
    if (!input)
        throw std::invalid_argument("calc_lexer::calc_lexer: null pointer");
}

template <typename CharT>
inline auto calc_lexer<CharT>::make_token(token_ids id, const CharT* tok_begin, const CharT* tok_end, error_codes error, num_type num_val) -> token {
    assert(tok_begin >= in_begin);
    assert(tok_end <= in_begin + char_helper::strlen(in_begin));
    auto tok_len = static_cast<typename token::string_view::size_type>(tok_end - tok_begin);
    return token{id, num_val, {tok_begin, tok_len}, error};
}

template <typename CharT>
auto calc_lexer<CharT>::get_tok() -> token {
    while (isspace(*in_pos)) // eat whitespace
        ++in_pos;

    if (char_helper::isalpha(*in_pos) || *in_pos == '_') { // identifier
        auto begin_pos = in_pos;  
        ++in_pos;
        while (char_helper::isalnum(*in_pos) || *in_pos == '_')
            ++in_pos;
        return make_token(token::identifier, begin_pos, in_pos, error_codes::no_error);
    }

    { // number
        auto tok_begin = in_pos;
        auto result = scan_as_number();
        if (in_pos != tok_begin)
            return make_token(token::number, tok_begin, in_pos, result.error, result.num_val);
        assert(result.error == error_codes::no_error);
    }

    token_ids tok_id;
    auto error = error_codes::no_error;
    auto begin_pos = in_pos;  
    switch (*in_pos) {
    case '+':
        ++in_pos;
        tok_id = token::add;
        break;
    case '-':
        ++in_pos;
        tok_id = token::sub;
        break;
    case '*':
        ++in_pos;
        if (*in_pos == '*') {
            tok_id = token::pow;
            ++in_pos;
        } else
            tok_id = token::mul;
        break;
    case '/':
        ++in_pos;
        tok_id = token::div;
        break;
    case '%':
        ++in_pos;
        tok_id = token::mod;
        break;
    case '(':
        ++in_pos;
        tok_id = token::lparen;
        break;
    case ')':
        ++in_pos;
        tok_id = token::rparen;
        break;
    case '=':
        ++in_pos;
        tok_id = token::eq;
        break;
    case '!':
        while (*++in_pos == '!')
            ;
        if (auto n = in_pos - begin_pos; n == 1) // factorial
            tok_id = token::fac;
        else if (n == 2) // double factorial
            tok_id = token::dfac;
        else { // multifactorial
            tok_id = token::unspecified;
            error = error_codes::mfac_unsupported;
        }
        break;
    case '<': // << arithmetic left shift (ashifl); <<< logical left shift (lshiftl)
        ++in_pos;
        if (*in_pos == '<') {
            ++in_pos;
            if (*in_pos == '<') {
                ++in_pos;
                tok_id = token::lshiftl;
            } else
                tok_id = token::ashiftl;
        } else
            tok_id = token::unspecified;
        break;
    case '>': // >> arithmetic right shift (ashifr); >>> logical right shift (lshiftr)
        ++in_pos;
        if (*in_pos == '>') {
            ++in_pos;
            if (*in_pos == '>') {
                ++in_pos;
                tok_id = token::lshiftr;
            } else
                tok_id = token::ashiftr;
        } else
            tok_id = token::unspecified;
        break;
    case '&': // bitwise and
        ++in_pos;
        tok_id = token::band;
        break;
    case '|': // bitwise or
        ++in_pos;
        tok_id = token::bor;
        break;
    case '^': // bitwise xor
        ++in_pos;
        tok_id = token::bxor;
        break;
    case '~': // bitwise not
        ++in_pos;
        tok_id = token::bnot;
        break;
    case ',':
        ++in_pos;
        tok_id = token::comma;
        break;
    case 0:
        tok_id = token::end;
        break;
    default:
        ++in_pos;
        tok_id = token::unspecified;
        break;
    }

    return make_token(tok_id, begin_pos, in_pos, error);
}

template <typename CharT>
auto calc_lexer<CharT>::scan_as_number() noexcept -> scan_result {
    num_buf.clear(); // we assume num_buf grows efficiently

    radices radix = default_radix;
    auto begin_pos = in_pos;

    bool has_radix_prefix = false;
    if (CharT code = 0; *in_pos == '0' &&
            char_helper::isalpha(char_helper::tolower(code = in_pos[1]))) { // radix code
        switch (code) {
        case 'b':
            // ambiguity: in default hex mode, 0b is also a valid hex number;
            // we'll favor interpreting as hex, meaning can't enter binary
            // numbers in hex mode
            if (default_radix != radices::base16) {
                radix = radices::base2;
                in_pos += 2;
                has_radix_prefix = true;
            }
            break;
        case 'o':
            radix = radices::base8;
            in_pos += 2;
            has_radix_prefix = true;
            break;
        case 'd':
            radix = radices::base10;
            in_pos += 2;
            has_radix_prefix = true;
            break;
        case 'x':
            radix = radices::base16;
            in_pos += 2;
            has_radix_prefix = true;
            break;
        // default: proceed incase code is a valid digit (e.g., a-f for default hex input)
        }
    }

    bool has_leading_zeros = *in_pos == '0';
    if (has_leading_zeros) { 
    // prevent C conversion routine from seeing an octal or radix prefix (hex
    // float exception will be handled later)
        do ++in_pos;
        while (*in_pos == '0');
    }

    auto isdigit = [&](CharT c) -> bool {
        auto digits = "0123456789abcdefghijklmnopqrstuvwxyz"sv;
        assert(radix <= digits.size());
        c = char_helper::tolower(c);
        auto radix_ = radix == radices::decimal ? radices::base10 : radix;
        auto digits_end = digits.begin() + (radix_ < radices::base10 ? radices::base10 : radix_);
        c = char_helper::tolower(c);
        for (auto itr = digits.begin(); itr != digits_end; ++itr)
            if (c == *itr)
                return true;
        return false;
    };

    string_view whole_part;
    if (isdigit(*in_pos)) {
        auto in_pos_ = in_pos;
        do ++in_pos;
        while (isdigit(*in_pos));
        whole_part = string_view(in_pos_, in_pos - in_pos_);
    }

    bool is_floating_point_number = false;
    string_view fractional_part;
    if (*in_pos == '.') {
        auto in_pos2 = in_pos + 1;
        if (isdigit(*in_pos2)) {
            do ++in_pos2;
            while (isdigit(*in_pos2));
            fractional_part = string_view(in_pos, in_pos2 - in_pos);
        }
        if (has_leading_zeros || whole_part.size() || fractional_part.size()) {
            in_pos = in_pos2;
            is_floating_point_number = true;
        }
    }

    if ((!has_leading_zeros && !whole_part.size() && !fractional_part.size())) {
        error_codes error;
        if (has_radix_prefix)
            error = error_codes::invalid_number;
        else {
            error = error_codes::no_error;
            in_pos = begin_pos; // not a number; back out and let this be handled elsewhere
        }
        return {float_type(), error};
    }

    string_view exponent_part;
    if (*in_pos == 'e' || *in_pos == 'E'
            || *in_pos == 'p' || *in_pos == 'P') { // exponent? (p/P for hexadecimal)
        auto in_pos2 = in_pos + 1;
        if (*in_pos2 == '+' || *in_pos2 == '-') // optional
            ++in_pos2;
        if (char_helper::isdigit(*in_pos2)) { // always decimal digits
            do ++in_pos2;
            while (char_helper::isdigit(*in_pos2));
            exponent_part = string_view(in_pos, in_pos2 - in_pos);
            in_pos = in_pos2;
            is_floating_point_number = true;
        }
    }

    if (radix == radices::base16 && is_floating_point_number) {
    // if floating point number, tell conversion function (strtod) to interpret as hex float
        num_buf += '0';
        num_buf += 'x';
    }
    if (!whole_part.size() && has_leading_zeros) // skipped leading zeros; include one of them
        num_buf += '0';
    else
        num_buf += whole_part;
    num_buf += fractional_part;
    num_buf += exponent_part;

    scan_result result;
    if (radix == radices::decimal ||
            (radix == radices::base10 && is_floating_point_number) ||
            (radix == radices::base16 && is_floating_point_number))
        result = scan_helper_float(num_buf.c_str());
    else if (is_floating_point_number)
        result = {int_type(), error_codes::integer_expected};
    else
        result = scan_helper_int(num_buf.c_str(), radix);
    return result;
}

template <typename CharT>
auto calc_lexer<CharT>::scan_helper_int(const CharT* in_pos, unsigned radix) noexcept -> scan_result {
// in_pos: position to begin scanning at.
// radix: number base (e.g., binary = 2, octal = 8, decimal = 10, hexadecimal = 16).
    assert(radix && radix <= 36); // 0-9 + a-z
    assert(radix != 32); // might not suitable for base 32; see https://en.wikipedia.org/wiki/Base32

    errno = 0; // global (thread-local) error code set by strtoll!; must initialize!
    auto int_val = char_helper::strtoull(in_pos, in_pos, radix);
    auto error = error_codes::no_error;
    if (errno) {
        if (errno == ERANGE)
            error = error_codes::out_of_range;
        else {
            assert(false); // missed a case
            error = error_codes::invalid_number;
        }
    } else if (*in_pos) // incomplete scan
        error = error_codes::invalid_number;
    return {int_val, error};
}

template <typename CharT>
auto calc_lexer<CharT>::scan_helper_float(const CharT* in_pos) noexcept -> scan_result {
    errno = 0; // global (thread-local) error code set by strtod!; must initialize!
    auto dbl_val = char_helper::strtod(in_pos, in_pos);
    auto error = error_codes::no_error;
    if (errno) {
        if (errno == ERANGE)
            error = error_codes::out_of_range;
        else {
            assert(false); // missed a case
            error = error_codes::invalid_number;
        }
    } else if (*in_pos) // incomplete scan
        error = error_codes::invalid_number;
    return {dbl_val, error};
}

#endif // CALC_LEXER_H