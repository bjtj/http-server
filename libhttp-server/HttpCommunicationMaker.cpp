#include "HttpCommunicationMaker.hpp"
#include "HttpCommunication.hpp"

namespace http {

	using namespace osl;

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
