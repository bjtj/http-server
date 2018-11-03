#ifndef __HTTP_HEADER_HPP__
#define __HTTP_HEADER_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

#include <liboslayer/StringElements.hpp>
#include "HttpParameter.hpp"

namespace http {

    /**
     * @breif HttpHeader
     */
    class HttpHeader {
    private:
	std::string part1;
	std::string part2;
	std::string part3;
	osl::LinkedStringListMap fields;
    public:
	HttpHeader();
	HttpHeader(const std::string & part1, const std::string & part2, const std::string & part3);
	virtual ~HttpHeader();
	std::string getFirstLine() const;
	virtual void clear();
	virtual void setHeader(const HttpHeader & other);
	virtual void setParts(const std::vector<std::string> & parts);
	virtual void setParts(const std::string & part1, const std::string & part2, const std::string & part3);
	virtual std::string getPart1() const;
	virtual std::string getPart2() const;
	virtual std::string getPart3() const;
	virtual void setPart1(const std::string & part);
	virtual void setPart2(const std::string & part);
	virtual void setPart3(const std::string & part);
	virtual bool hasHeaderFieldCase(const std::string & name) const;
	virtual std::string getHeaderFieldCase(const std::string & name) const;
	virtual bool hasHeaderField(const std::string & name) const;
	virtual std::string getHeaderField(const std::string & name) const;
	virtual int getHeaderFieldCaseAsInteger(const std::string & name) const;
	virtual int getHeaderFieldAsInteger(std::string name) const;
	virtual void setHeaderField(const std::string & name, const std::string & value);
	virtual void setHeaderField(const std::string & name, const osl::StringList & value);
	virtual void setHeaderField(const std::string & name, const std::vector<std::string> & value);
	virtual void setHeaderFields(const std::map<std::string, std::string> & fields);
	virtual void appendHeaderField(const std::string & name, const std::string & value);
	virtual void appendHeaderFields(const osl::LinkedStringMap & fields);
	virtual void appendHeaderFields(const std::map<std::string, std::string> & fields);
	virtual osl::StringList getHeaderFields(const std::string & name);
	virtual osl::LinkedStringListMap getHeaderFields();
	virtual std::map<std::string, std::string> getHeaderFieldsStdMap();
	virtual void removeHeaderFieldCase(const std::string & name);
	virtual void removeHeaderField(const std::string & name);
	virtual void removeHeaderFieldsCase(const std::string & name);
	virtual void removeHeaderFields(const std::string & name);

        /* HTTP */
	virtual std::string getContentType() const;
	virtual void setContentType(std::string contentType);
	virtual int getContentLength() const;
	virtual void setContentLength(long long contentLength);
	virtual bool isChunkedTransfer() const;
	virtual void setChunkedTransfer(bool chunked);
        virtual void setConnection(const std::string & connection);
        virtual bool keepConnection();
	virtual std::string toString() const;
	std::string & operator[] (const std::string & fieldName);
	const std::string operator[] (const std::string & fieldName) const;
    };


    /**
     * @breif HttpRequestHeader
     */
    class HttpRequestHeader : public HttpHeader {
    private:
	std::string resourcePath;
	std::map<std::string, HttpParameter> params;
	std::string fragment;
    public:
	HttpRequestHeader();
	HttpRequestHeader(const HttpHeader & other);
	virtual ~HttpRequestHeader();
	virtual void clear();
        virtual void setHeader(const HttpHeader & other);
	std::string getMethod() const;
	void setMethod(const std::string & method) ;
	std::string getPath() const;
	void setPath(const std::string & path) ;
	std::string getDirectory() const;
	std::string getRawPath() const;
	std::string getProtocol() const;
	void setProtocol(const std::string & protocol) ;
	std::string extractResourcePath(const std::string & path);
	std::string extractWithoutSemicolon(const std::string & path);
	std::string extractWithoutQuery(const std::string & path);
	std::string extractQuery(const std::string & path);
	std::string extractWithoutFragment(const std::string & path);
	std::string extractFragment(const std::string & path);
	std::vector<osl::KeyValue> parseSemiColonParameters(const std::string & path);
	osl::KeyValue parseKeyValue(const std::string & text);
	void parsePath(const std::string & path);
	void parseQuery(const std::string & query);
        std::vector<std::string> getParameterNames();
	bool hasParameter(const std::string & name);
	std::string getParameter(const std::string & name);
	std::vector<std::string> getParameters(const std::string & name);
	void setParameter(const std::string & name, const std::string & value);
	void setParameters(std::vector<osl::KeyValue> & nvs);
        void setHost(const std::string & host);
    };

    /**
     * @breif HttpResponseHeader
     */
    class HttpResponseHeader : public HttpHeader {
    private:
    public:
	HttpResponseHeader();
	HttpResponseHeader(int statusCode);
	HttpResponseHeader(int statusCode, const std::string & statusString);
	HttpResponseHeader(const HttpHeader & other);
	virtual ~HttpResponseHeader();
	virtual void clear();
	void init();
	std::string getProtocol() const;
	void setProtocol(const std::string & protocol);
	void setStatus(int statusCode);
	void setStatus(int statusCode, const std::string & statusString);
	int getStatusCode() const;
	void setStatusCode(int statusCode);
	std::string getStatusString() const;
	void setStatusString(const std::string & statusString);
        bool isRedirectionStatus();
        std::string getLocation();
    };
}

#endif
