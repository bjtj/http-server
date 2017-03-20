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
		UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher;
	public:
		HttpCommunicationMaker(UTIL::AutoRef<HttpRequestHandlerDispatcher> dispatcher);
		virtual ~HttpCommunicationMaker();
		virtual UTIL::AutoRef<Communication> makeCommunication();
	};
}

#endif
