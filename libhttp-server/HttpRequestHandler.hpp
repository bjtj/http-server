#ifndef __HTTP_REQUEST_HANDLER_HPP__
#define __HTTP_REQUEST_HANDLER_HPP__

#include "HttpHeader.hpp"
#include "HttpHeaderReader.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "ChunkedReader.hpp"
#include "DataTransfer.hpp"

#include <liboslayer/os.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Text.hpp>

namespace HTTP {

	/**
	 * @brief HttpRequestHandler
	 */
	class HttpRequestHandler {
	private:
	public:

		HttpRequestHandler();
		virtual ~HttpRequestHandler();

		virtual OS::AutoRef<DataSink> getDataSink();
    
		virtual void onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response);
        virtual void onHttpRequestContentCompleted(HttpRequest & request, OS::AutoRef<DataSink> sink, HttpResponse & response);
		virtual void onHttpResponseHeaderCompleted(HttpRequest & request, HttpResponse & response);
		virtual void onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response);
		virtual bool onException(HttpRequest & request, HttpResponse & response, OS::Exception & ex);
	};
}

#endif
