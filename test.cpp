#include "calc_outputter.h"
#include <iostream>
#include <vector>

int main() {
    using namespace std;
    using parser_type = tpcalc::parser<char>;
    using parse_error = tpcalc::parse_error<char>;
    using internal_error = tpcalc::internal_error;

    parser_type parser;
    vector<char> line_buf; // use vector for input line buffer for its memory efficiency
    line_buf.reserve(64); // initial capacity to mitigate memory reallocations
    cout.precision(15);

    tpcalc::outputter<char> outputter{parser.default_radix()};

    for (;; line_buf.clear()) {
        for (char c; cin.get(c) && c != '\n';)
            line_buf.emplace_back(c);
        line_buf.emplace_back(0); // null terminator

        try {
            if (!parser.evaluate(line_buf.data()))
                break;
            cout << outputter(parser.last_val()) << endl;
        } catch (const parse_error& e) {
            cout << e.error_str() << endl;
            if (e.view_is_valid_for(line_buf.data())) {
                cout << line_buf.data() << endl;
                for (auto n = e.tok.tok_str.data() - line_buf.data(); n; --n)
                    cout << '.';
                cout << "^" << endl;
            }
        } catch (const internal_error& e) {
            cout << "Unexpected error in " << e.str << '.' << endl;
        }
    }
}
