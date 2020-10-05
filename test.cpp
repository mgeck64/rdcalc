#include "calc_outputter.h"
#include <iostream>
#include <vector>

int main() {
    using namespace std;
    using parser_t = calc_parser<char>;

    parser_t parser;
    vector<char> line_buf; // use vector for input line buffer for its memory efficiency
    line_buf.reserve(64); // initial capacity to mitigate memory reallocations
    cout.precision(15);

    calc_outputter<char> outputter{parser.default_radix()};

    for (;; line_buf.clear()) {
        for (char c; cin.get(c) && c != '\n';)
            line_buf.emplace_back(c);
        line_buf.emplace_back(0); // null terminator

        try {
            if (!parser.eval(line_buf.data()))
                break;
            cout << outputter(parser.last_val()) << endl;
        } catch (const parser_t::parse_error& e) {
            cout << e.error_str() << endl;
            if (e.view_is_valid_for(line_buf.data())) {
                cout << line_buf.data() << endl;
                for (auto n = e.tok.tok_str.data() - line_buf.data(); n; --n)
                    cout << '.';
                cout << "^" << endl;
            }
        }
    }
}
