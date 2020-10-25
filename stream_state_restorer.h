#pragma once
#ifndef STREAM_STATE_RESTORER_H
#define STREAM_STATE_RESTORER_H

#include <ios>

namespace tpcalc {

class stream_state_restorer {
	std::ios_base& stream;
	std::ios_base::fmtflags flags;
	std::streamsize precision;

public:
	stream_state_restorer(std::ios_base& stream_) :
		stream{stream_},
		flags{stream.flags()},
		precision{stream.precision()}
	{}

	~stream_state_restorer() {
		stream.flags(flags);
		stream.precision(precision);
	}
};

} // namespace tpcalc

#endif // TEMP_STREAM_STATE_H
