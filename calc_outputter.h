#pragma once
#ifndef CALC_OUTPUTTER_H
#define CALC_OUTPUTTER_H

#include "calc_parser.h"
#include "ieee_double.h"
#include "stream_state_restorer.h"
#include <ostream>

template <typename CharT>
class calc_outputter {
public:
	using parser_type = calc_parser<CharT>;
	using val_type = typename parser_type::val_type;
	using float_type = typename parser_type::float_type;
	using widest_uint_type = typename parser_type::widest_uint_type;
	using widest_int_type = typename parser_type::widest_int_type;
	using radices = typename parser_type::radices;
	using ostream = std::basic_ostream<CharT>;

	calc_outputter(radices radix) : radix_{radix}, output_fn{output_fn_for(radix)} {}
	calc_outputter() = default;
	calc_outputter(const calc_outputter&) = default;

	void radix(radices radix);
	auto radix() {return radix_;}
	const calc_outputter& operator()(const val_type& val_var_) {val_var = val_var_; return *this;}
	friend ostream& operator<<(ostream& out, const calc_outputter& outputter) {return outputter.output_fn(out, outputter.val_var);}

private:
	val_type val_var = float_type(0);
	radices radix_ = radices::decimal;

	static auto output_bin(ostream& out, const val_type& val_var) -> ostream&;
	static auto output_oct(ostream& out, const val_type& val_var) -> ostream&;
	static auto output_dec(ostream& out, const val_type& val_var) -> ostream&;
	static auto output_hex(ostream& out, const val_type& val_var) -> ostream&;

	using output_fn_type = auto (*)(ostream& out, const val_type& val_var) -> ostream&;
	output_fn_type output_fn = output_dec; // (note: auto not allowed for non-static members!)
	static auto output_fn_for(radices radix) -> output_fn_type;

	static auto output(ostream& out, const val_type& val_var, unsigned radix) -> ostream&;
	static auto output_as_uint(ostream& out, widest_uint_type val, unsigned radix) -> ostream&;
	static auto output_as_ieee_double(ostream& out, float_type val, unsigned radix) -> ostream&;

	template <typename OutValFn>
	static auto output(OutValFn out_val_fn, ostream& out, const typename parser_type::list_type& list) -> ostream&;

	static constexpr auto digits = std::array{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
		'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};
};

template <typename CharT>
inline void calc_outputter<CharT>::radix(radices radix) {
	radix_ = radix;
	output_fn = output_fn_for(radix);
}

template <typename CharT>
auto calc_outputter<CharT>::output_fn_for(radices radix) -> output_fn_type {
	switch (radix) {
	case radices::base2:
		return output_bin;
	case radices::base8:
		return output_oct;
	case radices::base16:
		return output_hex;
	default:
		assert(false); // missed one; fall thru to base10 case
		[[fallthrough]];
	case radices::decimal:
	case radices::base10:
		return output_dec;
		break;
	}
}

template <typename CharT>
inline auto calc_outputter<CharT>::output_bin(ostream& out, const val_type& val_var) -> ostream& {
	return output(out, val_var, 2);
}

template <typename CharT>
inline auto calc_outputter<CharT>::output_oct(ostream& out, const val_type& val_var) -> ostream& {
	return output(out, val_var, 8);
}

template <typename CharT>
auto calc_outputter<CharT>::output_dec(ostream& out, const val_type& val_var) -> ostream& {
	stream_state_restorer restorer(out);
	out.precision(std::numeric_limits<float_type>::digits10);
	return std::visit([&](auto val) -> ostream& {
		if constexpr (std::is_same_v<decltype(val), float_type>)
			return out << std::defaultfloat << val;
		else if constexpr (std::is_signed_v<decltype(val)>) {
			static_assert(sizeof(val) <= sizeof(widest_int_type));
			return out << std::dec << static_cast<widest_int_type>(val); // cast ensures char type will be output as int
		} else if constexpr (std::is_unsigned_v<decltype(val)>) {
			static_assert(sizeof(val) <= sizeof(widest_uint_type));
			return out << std::dec << static_cast<widest_uint_type>(val); // ditto
		} else {
			static_assert(std::is_same_v<decltype(val), parser_type::list_type>);
			return output([](ostream& out, const val_type& val_var) -> ostream& {
				return output_dec(out, val_var);
			}, out, val);
		}
	}, val_var);
}

template <typename CharT>
auto calc_outputter<CharT>::output_hex(ostream& out, const val_type& val_var) -> ostream& {
	return output(out, val_var, 16);
}

template <typename CharT>
auto calc_outputter<CharT>::output(ostream& out, const val_type& val_var, unsigned radix) -> ostream& {
	return std::visit([&](const auto& val) -> ostream& {
		using VT = std::decay_t<decltype(val)>;
		if constexpr (std::is_integral_v<VT>) {
			auto pval = reinterpret_cast<const std::make_unsigned_t<VT>*>(&val);
			static_assert(sizeof(val) == sizeof(*pval));
			static_assert(sizeof(widest_uint_type) >= sizeof(*pval));
			return output_as_uint(out, static_cast<widest_uint_type>(*pval), radix);
		} else if constexpr (std::is_same_v<VT, float_type>)
			return output_as_ieee_double(out, val, radix);
		else {
			static_assert(std::is_same_v<VT, parser_type::list_type>);
			return output([&](ostream& out, const val_type& val_var) -> ostream& {
				return output(out, val_var, radix);
			}, out, val); 
		}
	}, val_var);
}

template <typename CharT>
auto calc_outputter<CharT>::output_as_uint(ostream& out, widest_uint_type val, unsigned radix) -> ostream& {
	unsigned digit_break = 0;
	decltype(val) mask = 0;
	size_t shift = 0;
	if (radix == 2) {
		digit_break = 4;
		mask = 1;
		shift = 1;
	} else if (radix == 8) {
		digit_break = 3;
		mask = 7;
		shift = 3;
	} else if (radix == 16) {
		digit_break = 4;
		mask = 15;
		shift = 4;
	} else // unsupported
		assert(false);

	widest_uint_type reversed = 0;
	unsigned digit_count = 0;
	while (val >= radix) { // all digits except leftmost one
		reversed <<= shift;
		reversed |= val & mask;
		val >>= shift;
		++digit_count;
	}
	
	// leftmost digit (or 0) -- leftmost digit may not be "full"; e.g., for
	// octal, 64%3 == 1: 64 is bit width of val and 3 is bit width of octal
	// digit
	assert(val < digits.size());
	out << digits.at(static_cast<size_t>(val));
	
	// remaining reversed digits
	assert(mask < digits.size());
	while (digit_count) {
		if (digit_break && !(digit_count % digit_break))
			out << ' ';
		out << digits.at(static_cast<size_t>(reversed & mask));
		reversed >>= shift;
		--digit_count;
	}

	return out;
}

template <typename CharT>
auto calc_outputter<CharT>::output_as_ieee_double(ostream& out, float_type val, unsigned radix) -> ostream& {
	auto ieee_val = ieee_double(val);

    if (ieee_val.is_negative())
        out << '-';

    if (ieee_val.is_inf())
        out << "inf";
    else if (ieee_val.is_nan())
        out << "nan";
    else {
        auto exponent = ieee_val.exponent();
        if (ieee_val.is_subnormal()) {
            exponent += 1;
            out << '0';
        } else if (ieee_val.is_actual_zero()) {
            exponent = 0;
            out << '0';
        } else
            out << '1';

        if (ieee_val.significand() != 0) {
            out << '.';

			auto significand = ieee_val.significand();
			decltype(significand) pilot = 0xfffffffffffff; // this will cause leading 0's in significand (i.e., fraction) to be preserved
			size_t mask = 0;
			decltype(significand) reversed = 0;
			size_t last = 0;

			size_t shift = 0;
			if (radix == 2) {
				mask = 1;
				shift = 1;
			} else if (radix == 8) {
				mask = 7;
				shift = 3;
				// special case: last (rightmost) digit is only 1 bit wide
				// (52%3 == 1; 52 is bit width of significand field; 3 is bit width of octal digit)
				last = significand & 1;
				significand >>= 1;
				pilot >>= 1;
			} else if (radix == 16) {
				mask = 15;
				shift = 4;
			} else // unsupported
				assert(false);

			while (pilot) {
				reversed <<= shift;
				reversed |= significand & mask;
				significand >>= shift;
				pilot >>= shift;
			}

			assert(mask < digits.size());
			while (reversed) { // effectively outputs significand (as reversed) with trailing 0's trimmed
				out << digits.at(reversed & mask);
				reversed >>= shift;
			}

			if (last) { // test to prevent trailing 0
				assert(last < digits.size());
				out << digits.at(last);
			}

			// note: can't use output_as_uint for this because this algorithm
			// basically interprets significand left-to-right
        }

        out << 'p';
        if (exponent >= 0)
            out << '+';
        out << std::dec << exponent;
    }

    return out;
}

template <typename CharT>
template <typename OutValFn>
auto calc_outputter<CharT>::output(OutValFn out_val_fn, ostream& out, const typename parser_type::list_type& list) -> ostream& {
	if (!list.size())
		out << "none";
	else 
		for (auto itr = list.begin(); itr != list.end(); ++itr) {
			if (itr != list.begin())
				out << ',';
			out_val_fn(out, *itr);
		}
	return out;
}

#endif // CALC_OUTPUTTER_H