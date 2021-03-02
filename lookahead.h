#pragma once
#ifndef LOOKAHEAD_H
#define LOOKAHEAD_H

#include "calc_lexer.h"
#include "parse_error.h"

namespace tpcalc {

template <typename CharT>
class lookahead_lexer {
// simulates two-token lookhead lexer using lexer. (implemented
// separately from lexer to keep lexer clean and simple, and to
// encapsulate lookahead logic)
public:
    using token = token<CharT>;
    using token_ids = typename token::token_ids;
    using radices = typename token::radices;
    using parse_error = parse_error<CharT>;

    lookahead_lexer(const CharT* input, const radices& default_radix) :
        lexer{input, default_radix} {}
    lookahead_lexer() = default;

    auto get_tok() -> const token&; // consume a token; throws parse_error if token has error
    auto cached_tok() const -> const token& {return cached_tok_;}

    auto get_expected_tok(token_ids id) -> const token&;
    // consumes a token; throws parse_error if id doesn't match the consumed
    // token's id

    auto peek_tok() -> const token&; // peek at but don't consume token
    auto peeked_tok() const -> const token& {return peeked_tok_;}

    auto peek_tok2() -> const token&; // peek at second token
    auto peeked_tok2() const -> const token& {return peeked_tok2_;}

private:
    lexer<CharT> lexer;
    unsigned peeked = 0;
    token peeked_tok_ = {};
    token peeked_tok2_ = {};
    token cached_tok_ = {};
};

template <typename CharT>
inline auto lookahead_lexer<CharT>::peek_tok() -> const token& {
    if (!peeked) {
        peeked_tok_ = lexer.get_tok();
        peeked = 1;
    }
    return peeked_tok_;
}

template <typename CharT>
inline auto lookahead_lexer<CharT>::peek_tok2() -> const token& {
    if (peeked != 2) {
        peek_tok();
        peeked_tok2_ = lexer.get_tok();
        peeked = 2;
    }
    return peeked_tok2_;
}

template <typename CharT>
inline auto lookahead_lexer<CharT>::get_tok() -> const token& {
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
        throw parse_error(parse_error::lexer_error, cached_tok_);
    return cached_tok_;
}

template <typename CharT>
inline auto lookahead_lexer<CharT>::get_expected_tok(token_ids id) -> const token& {
    if (get_tok().id != id)
        throw parse_error(parse_error::tok_expected, cached_tok_, id);
    return cached_tok_;
}

} // namespace tpcalc

#endif // LOOKAHEAD_H