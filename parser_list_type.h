#pragma once
#ifndef PARSER_LIST_TYPE_H
#define PARSER_LIST_TYPE_H

#include "parser_basics.h"
#include <vector>

namespace tpcalc {

struct list_type : public std::vector<parser_num_type> {
    template <typename T>
    list_type(const T& val) // convert val to a one element list of val
        : std::vector<parser_num_type>(1, val) {}

    list_type() = default;
    list_type(const list_type&) = default; // make sure template ctor doesn't generate this case
};

} // namespace tpcalc

#endif // PARSER_LIST_TYPE_H