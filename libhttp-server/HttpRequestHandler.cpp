#include "HttpRequestHandler.hpp"
#include "StringDataSink.hpp"
#include <liboslayer/Logger.hpp>
#include <liboslayer/File.hpp>


namespace http {

	using namespace osl;

	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));

	/**
	 * @brief HttpRequestHandler
	 */

	HttpRequestHandler::HttpRequestHandler() {
	}
	
	HttpRequestHandler::~HttpRequestHandler() {
	}

	AutoRef<DataSink> HttpRequestHandler::getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}

	void HttpRequestHandler::onHttpRequestHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		// request header transfer done
	}
	
	void HttpRequestHandler::onHttpRequestContentCompleted(HttpRequest & request,
														   AutoRef<DataSink> sink,
														   HttpResponse & response) {
		// request content transfer done
	}
	
	void HttpRequestHandler::onHttpResponseHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		// response header transfer done
	}
	
	void HttpRequestHandler::onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response) {
		// response content transfer done
	}
	
	bool HttpRequestHandler::onException(HttpRequest & request, HttpResponse & response, Exception & ex) {
		logger->error("Error message - " + ex.toString());
		return false;
	}
}
