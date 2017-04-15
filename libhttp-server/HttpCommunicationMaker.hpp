#ifndef __HTTP_COMMUNICATION_MAKER_HPP__
#define __HTTP_COMMUNICATION_MAKER_HPP__

#include "ConnectionManager.hpp"
#include "HttpRequestHandlerDispatcher.hpp"
#include <liboslayer/AutoRef.hpp>

namespace HTTP {

	/**
	 * @brief HttpCommunicationMaker
	 */
	class HttpCommunicationMaker : public CommunicationMaker {
	private:
		OS::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunicationMaker(OS::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunicationMaker();
		virtual OS::AutoRef<Communication> makeCommunication();
	};
}

#endif
