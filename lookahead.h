#pragma once
#ifndef LOOKAHEAD_H
#define LOOKAHEAD_H

// definitions for calc_parser::lookahead

#include "calc_parser.h"

template <typename CharT>
inline auto calc_parser<CharT>::lookahead::peek_tok() -> const token& {
    if (!peeked) {
        peeked_tok_ = lexer.get_tok();
        peeked = 1;
    }
    return peeked_tok_;
}

template <typename CharT>
inline auto calc_parser<CharT>::lookahead::peek_tok2() -> const token& {
    if (peeked != 2) {
        peek_tok();
        peeked_tok2_ = lexer.get_tok();
        peeked = 2;
    }
    return peeked_tok2_;
}

template <typename CharT>
inline auto calc_parser<CharT>::lookahead::get_tok() -> const token& {
    if (!peeked)
        cached_tok_ = lexer.get_tok();
    else {
        cached_tok_ = peeked_tok_;
        if (--peeked) {
            peeked_tok_ = peeked_tok2_;
            peeked = 1;
        }
    }
    if (cached_tok_.error != token::no_error)
        throw make_parse_error(parse_error::lexer_error, cached_tok_);
    return cached_tok_;
}

template <typename CharT>
inline auto calc_parser<CharT>::lookahead::get_expected_tok(token_ids id) -> const token& {
    if (get_tok().id != id)
        throw make_parse_error(parse_error::tok_expected, cached_tok_, id);
    return cached_tok_;
}

#endif // LOOKAHEAD_H