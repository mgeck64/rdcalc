#pragma once
#ifndef PARSE_ERROR_H
#define PARSE_ERROR_H

// definitions for calc_parser::parse_error

#include "calc_parser.h"

template <typename CharT>
inline bool calc_parser<CharT>::parse_error::view_is_valid_for(const CharT* input) const {
    return (tok.tok_str.data() >= input)
        && (tok.tok_str.data() + tok.tok_str.size() <= input + char_helper::strlen(input));
}

template <typename CharT>
auto calc_parser<CharT>::parse_error::error_str() const -> string {
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
    } else if (!error_txt_prefix_len || tok.tok_str.size()) { // !error_txt_prefix_len: error_txt_ can't be shown independently of the token, so show it even if blank
        char_helper::append_to(error_str_buf, "\"");
        char_helper::append_to(error_str_buf, tok.tok_str);
        char_helper::append_to(error_str_buf, "\" ");
        token_shown = true;
    }

    if (!token_shown)
        error_txt_ += error_txt_prefix_len;
    char_helper::append_to(error_str_buf, error_txt_);
    char_helper::append_to(error_str_buf, ".");

    return error_str_buf;
}

#endif // PARSE_ERROR_H