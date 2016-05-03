#include "FixedTransfer.hpp"

namespace HTTP {
    
    using namespace std;
	using namespace OS;
    using namespace UTIL;
    
    /**
     * @brief FixedTransfer
     */
    
	FixedTransfer::FixedTransfer(AutoRef<DataSource> source, size_t size) : DataTransfer(source), indicator(size) {
	}
	FixedTransfer::FixedTransfer(AutoRef<DataSink> sink, size_t size) : DataTransfer(sink), indicator(size) {
	}
    FixedTransfer::~FixedTransfer() {
    }
	void FixedTransfer::recv(Connection & connection) {

		if (sink().nil()) {
			throw Exception("sink required");
		}

		connection.setReadSize(indicator.adjustReadSize(connection.getLimit()));
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
			size_t size = indicator.adjustReadSize(1024);
			char * buffer = new char[size];
			size_t readlen = source()->read(buffer, size);
			try {
				size_t sendlen = connection.send(buffer, readlen);
				indicator.offset(sendlen);
				delete [] buffer;
			} catch (IOException & e) {
				delete [] buffer;
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
