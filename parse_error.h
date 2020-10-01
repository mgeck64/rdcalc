#pragma once
#ifndef PARSE_ERROR_H
#define PARSE_ERROR_H

// definitions for calc_parser::parse_error

#include "calc_parser.h"

template <typename CharT>
inline void calc_parser<CharT>::parse_error::assert_view_is_valid_for(const CharT* input) const {
    assert(tok.tok_str.data() >= input);
    assert(tok.tok_str.data() + tok.tok_str.size() <= input + char_helper::strlen(input));
}

template <typename CharT>
auto calc_parser<CharT>::parse_error::error_str() const -> string {
    string error_str_buf;
    error_str_buf.reserve(64); // initial capacity to mitigate memory reallocations

    char_helper::append_to(error_str_buf, "Error: ");
    if (error == lexer_error) {
        assert(tok.error != token::no_error);
        char_helper::append_to(error_str_buf, token::error_txt.at(tok.error));
    } else {
        assert(error != token::no_error);
        if (error == tok_expected) {
            char_helper::append_to(error_str_buf, token::token_txt.at(expected_tok));
            char_helper::append_to(error_str_buf, " ");
        }
        char_helper::append_to(error_str_buf, error_txt.at(error));
    }
    char_helper::append_to(error_str_buf, ".");

    return error_str_buf;
}

#endif // PARSE_ERROR_H