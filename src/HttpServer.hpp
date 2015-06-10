#ifndef __HTTP_SERVER_HPP__
#define __HTTP_SERVER_HPP__

namespace HTTP {

	class HttpServer {
	private:
		int port;
        bool running;
	public:
		HttpServer(int port);
		virtual ~HttpServer();

		virtual void start();
		virtual void stop();
		virtual bool isRunning();
	};

}

#endif
