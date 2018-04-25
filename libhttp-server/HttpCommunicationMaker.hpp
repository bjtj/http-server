#ifndef __HTTP_COMMUNICATION_MAKER_HPP__
#define __HTTP_COMMUNICATION_MAKER_HPP__

#include "ConnectionManager.hpp"
#include "HttpRequestHandlerDispatcher.hpp"
#include <liboslayer/AutoRef.hpp>

namespace http {

	/**
	 * @brief HttpCommunicationMaker
	 */
	class HttpCommunicationMaker : public CommunicationMaker {
	private:
		osl::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunicationMaker(osl::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunicationMaker();
		virtual osl::AutoRef<Communication> makeCommunication();
	};
}

#endif
