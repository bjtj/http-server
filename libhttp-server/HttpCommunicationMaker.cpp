#include "HttpCommunicationMaker.hpp"
#include "HttpCommunication.hpp"

namespace HTTP {

	using namespace UTIL;

	/**
	 * @brief HttpCommunicationMaker
	 */
	
    HttpCommunicationMaker::HttpCommunicationMaker(AutoRef<HttpRequestHandlerDispatcher> dispatcher) : dispatcher(dispatcher) {
    }

    HttpCommunicationMaker::~HttpCommunicationMaker() {
    }
    
    AutoRef<Communication> HttpCommunicationMaker::makeCommunication() {
        return AutoRef<Communication>(new HttpCommunication(dispatcher));
    }

}
