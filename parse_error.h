#pragma once
#ifndef PARSE_ERROR_H
#define PARSE_ERROR_H

#include "calc_lexer.h"

namespace tpcalc {

template <typename CharT>
struct error_context {
    using token = token<CharT>;
    token tok; // warning: has string_view that may be bound to the input string or may be unbound default string view
};

template <typename CharT>
struct parse_error {
    using token = ::tpcalc::token<CharT>;
    using token_ids = typename token::token_ids;
    using string_view = std::basic_string_view<CharT>;
    using string = std::basic_string<CharT>;
    using error_context = error_context<CharT>;

    enum error_codes {
        no_error, lexer_error, syntax_error, number_expected,
        undefined_identifier, tok_expected,
        num_operand_expected, num_operands_expected,
        int_operand_expected, int_operands_expected,
        negative_shift_invalid, division_by_0, unexpected_error,
        operands_must_be_lists, lists_must_be_same_size,
        unexpected_end_of_input};
    static constexpr auto error_txt = std::array{
        // elements correspond with error_codes enums so enum can be used as index
        "- no_error", "- lexer error", "- syntax error", "- number expected",
        "is undefined", "was expected",
        "- non-numeric operand was given", "- non-numeric operand was given",
        "- integer operand expected", "- integer operands expected",
        "- negative shift value is invalid", "- division by 0", "- unexpected error",
        "- both operands must be lists", "- lists must be the same size",
        "- unexpected end of input"};

    error_codes error = no_error;
    token tok; // warning: has string_view that may be bound to the input string or may be unbound default string view
    string tok_str; // copy of tok.tok_str; can use safely instead of tok.tok_str
    token_ids expected_tok = token::none; // valid for error == tok_expected
    bool view_is_valid_for(const CharT* input) const; // is tok.tok_str bound to input?
    auto error_str() const -> string;

    parse_error(error_codes error_, const error_context& err_context);
    parse_error(error_codes error_, const token& tok_, token_ids expected_tok_ = token::unspecified);
};

template <typename CharT>
parse_error<CharT>::parse_error(error_codes error_, const error_context& err_context)
    : error{error_}, tok{err_context.tok}, expected_tok{}, tok_str{err_context.tok.tok_str}
{
    assert(expected_tok == token::unspecified || error == tok_expected);
}

template <typename CharT>
inline parse_error<CharT>::parse_error(error_codes error_, const token& tok_, token_ids expected_tok_)
// expected_tok != token::unspecified is only valid for error == tok_expected
    : error{error_}, tok{tok_}, expected_tok{expected_tok_}, tok_str{tok_.tok_str}
{
    assert(expected_tok == token::unspecified || error == tok_expected);
}

template <typename CharT>
inline bool parse_error<CharT>::view_is_valid_for(const CharT* input) const {
    return (tok.tok_str.data() >= input)
        && (tok.tok_str.data() + tok.tok_str.size() <= input + char_helper::strlen(input));
}

template <typename CharT>
auto parse_error<CharT>::error_str() const -> string {
    string error_str_buf;
    error_str_buf.reserve(64); // initial capacity to mitigate memory reallocations

    char_helper::append_to(error_str_buf, "Error: ");

    const char* error_txt_ = "";
    if (error == lexer_error) {
        assert(tok.error != token::no_error);
        error_txt_ = token::error_txt.at(tok.error);
    } else {
        assert(error != token::no_error);
        error_txt_ = error_txt.at(error);
    }

    // an error message (error_txt) may have a dash prefix, such as "- syntax
    // error", in which case it can be shown independently of the token, or it
    // may be something like "is undefined", in which case it can't be shown
    // independently of the token
    typename string_view::size_type error_txt_prefix_len = 0;
    if (*error_txt_ == '-') {
        ++error_txt_prefix_len;
        if (error_txt_[1] == ' ')
            ++error_txt_prefix_len;
    }

    bool token_shown = false;
    if (error == tok_expected) {
        assert(char_helper::strlen(token::token_txt.at(expected_tok)));
        char_helper::append_to(error_str_buf, token::token_txt.at(expected_tok));
        char_helper::append_to(error_str_buf, " ");
        token_shown = true;
    } else if (!error_txt_prefix_len || !tok_str.empty()) { // !error_txt_prefix_len: error_txt_ can't be shown independently of the token, so show it even if blank
        char_helper::append_to(error_str_buf, "\"");
        char_helper::append_to(error_str_buf, tok_str);
        char_helper::append_to(error_str_buf, "\" ");
        token_shown = true;
    }

    if (!token_shown)
        error_txt_ += error_txt_prefix_len;
    char_helper::append_to(error_str_buf, error_txt_);
    char_helper::append_to(error_str_buf, ".");

    return error_str_buf;
}

} // namespace tpcalc

#endif // PARSE_ERROR_H
