#include "FixedTransfer.hpp"

namespace HTTP {
    
    using namespace std;
	using namespace OS;
    using namespace UTIL;
    
    /**
     * @brief FixedTransfer
     */
    
	FixedTransfer::FixedTransfer(AutoRef<DataSource> source, size_t size)
		: DataTransfer(source), indicator(size), _size(size)
	{
		_buffer = new char[size];
	}
	
	FixedTransfer::FixedTransfer(AutoRef<DataSink> sink, size_t size)
		: DataTransfer(sink), indicator(size), _size(size)
	{
		_buffer = new char[size];
	}
	
    FixedTransfer::~FixedTransfer() {
		delete[] _buffer;
    }
	
	void FixedTransfer::recv(Connection & connection) {
		if (sink().nil()) {
			throw Exception("sink required");
		}
		connection.packet().setLimit(indicator.adjustReadSize(connection.packet().getLimit()));
		Packet & packet = connection.read();
		indicator.offset(sink()->write(packet.getData(), packet.getLength()));
		if (indicator.completed()) {
			complete();
		}
    }
	
    void FixedTransfer::send(Connection & connection) {
		if (source().nil()) {
			throw Exception("source required");
		}
		if (indicator.remain()) {
			size_t size = indicator.adjustReadSize(_size);
			size_t readlen = source()->read(_buffer, size);
			try {
				size_t sendlen = connection.send(_buffer, readlen);
				indicator.offset(sendlen);
			} catch (IOException & e) {
				throw e;
			}
		}

        if (indicator.completed()) {
			complete();
        }
    }

	unsigned long long FixedTransfer::size() {
		return indicator.size();
	}
}
