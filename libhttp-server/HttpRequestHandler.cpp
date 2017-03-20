#include "HttpRequestHandler.hpp"
#include "StringDataSink.hpp"
#include <liboslayer/Logger.hpp>

namespace HTTP {

	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

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
		logger->logi("Default onHttpRequestHeaderCompleted() invoked");
	}
	
	void HttpRequestHandler::onHttpRequestContentCompleted(HttpRequest & request,
														   AutoRef<DataSink> sink,
														   HttpResponse & response) {
		// request content transfer done
		logger->logi("Default onHttpRequestContentCompleted() invoked");
	}
	
	void HttpRequestHandler::onHttpResponseHeaderCompleted(HttpRequest & request, HttpResponse & response) {
		// response header transfer done
		logger->logi("Default onHttpResponseHeaderCompleted() invoked");
	}
	
	void HttpRequestHandler::onHttpResponseTransferCompleted(HttpRequest & request, HttpResponse & response) {
		// response content transfer done
		logger->logi("Default onHttpResponseTransferCompleted() invoked");
	}
	
	bool HttpRequestHandler::onException(HttpRequest & request, HttpResponse & response, Exception & ex) {
		logger->loge("Error message - " + ex.getMessage());
		return false;
	}
}
