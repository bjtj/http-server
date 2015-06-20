#include "os.hpp"

#include <iostream>

/**
 * @namespace OS
 */
namespace OS {

	using namespace std;


	/**
	 * @brief milliseconds sleep
	 */
	void idle(unsigned long timeout) {
#if defined(USE_UNIX_STD)
		usleep(timeout * 1000);
#elif defined(USE_MS_WIN)
		Sleep(timeout);
#else
		// sleep
#endif
	}

	/**
	 * @brief get tick count
	 */
	unsigned long tick_milli() {
#if defined(USE_UNIX_STD)

		struct timeval tv;
		if( gettimeofday(&tv, NULL) != 0 )
			return 0;
		
		return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
		
#elif defined(USE_MS_WIN)
		return GetTickCount();
#else
		return 0;
#endif	
	}


	/* SEMAPHORE */
#if defined(USE_POSIX_SEMAPHORE)

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		sem_init(handle, 0, initial);
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		sem_wait(handle);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		sem_post(handle);
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		sem_destroy(handle);
	}

#else

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		*handle = CreateSemaphore(
			NULL,		// default security attributes
			initial,	// initial count
			initial,	// maximum count
			NULL);		// unnamed semaphore
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		WaitForSingleObject(*handle, INFINITE);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		ReleaseSemaphore( 
                        *handle,	// handle to semaphore
                        1,			// increase count by one
                        NULL);		// not interested in previous count
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		CloseHandle(*handle);
	}

#endif /* SEMAPHORE */
	

	Semaphore::Semaphore(int initial) {
		s_sem_init(&handle, initial);
	}
	
	Semaphore::~Semaphore() {
		s_sem_destroy(&handle);
	}
	
	void Semaphore::wait() {
		s_sem_wait(&handle);
	}
	
	void Semaphore::post() {
		s_sem_post(&handle);
	}


	/* THREAD */
#if defined(USE_PTHREAD)

	// unix or linux

	typedef void * (*thread_func)(void *);
	
	static void * s_thread_wrapper(void * arg) {

		char name[16] = {0,};
		Thread * thread = (Thread*)arg;

		snprintf(name, sizeof(name), "tid:0x%x", (unsigned int)thread->getId());
		prctl(PR_SET_NAME, name, 0, 0, 0);

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief pthread_create
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread) {
	
		pthread_attr_t attr;
		bool started = false;

		if (!handle || !thread) {
			return false;
		}

		if (pthread_attr_init(&attr) != 0) {
			return false;
		}

		if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0) {
			return false;
		}
			
		if (pthread_create(handle, &attr, func, (void*)thread) == 0) {
			started = true;
		}

		pthread_attr_destroy(&attr);
		return started;
	}

#elif defined(USE_WIN_THREAD)

	// windows

	typedef UINT (WINAPI thread_func)(void *);

	static UINT WINAPI s_thread_wrapper(void * arg) {

		Thread * thread = (Thread*)arg;

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief win32 thread start
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread) {
		// TODO: 
		UINT dwThreadID;
		HANDLE h;

		if (!handle || !thread) {
			return false;
		}

		h = (HANDLE)_beginthreadex(NULL, 0, func, (void*)thread, 0, &dwThreadID);
		*handle = h;
		
		return false;
	}

#endif

	/**
	 * @brief thread id seed - 0 means error
	 */
	unsigned int Thread::s_thread_id_seed = 0;

	/**
	 * @brief thread constructor
	 */
	Thread::Thread() : handle(0), signal_interrupt(false) {
		id = (++s_thread_id_seed == 0) ? ++s_thread_id_seed : s_thread_id_seed;

		reset();
	}
	
	Thread::~Thread() {
	}

	unsigned int Thread::getId() {
		return id;
	}

	void Thread::reset() {
		signal_interrupt = false;
		running = false;
	}

	bool Thread::start() {
		
		if (!isRunning()) {
			bool ret = s_startThread(&handle, s_thread_wrapper, this);
			running = true;
			return ret;
		}
		return false;
	}
	
	void Thread::interrupt() {
		signal_interrupt = true;
	}

	bool Thread::interrupted() {
		bool ret = signal_interrupt;
		signal_interrupt = false;
		return ret;
	}

	bool Thread::isRunning() {
		return running;
	}

	void Thread::join() {
		while (running) { /* take time : 10ms */ idle(10); }
	}


	/* Network*/
	
#if defined(USE_BSD_SOCKET)

	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
	
		struct ifaddrs * ifAddrStruct = NULL;
		struct ifaddrs * ifa = NULL;
		void * tmpAddrPtr = NULL;
		getifaddrs( &ifAddrStruct );
		int found=0;

		for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
			if ( ifa->ifa_addr != NULL && ifa ->ifa_addr->sa_family == AF_INET ) { // check it is IP4
				// is a valid IP4 Address
				tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
				inet_ntop( AF_INET, tmpAddrPtr, ipAddressBuffer, INET_ADDRSTRLEN );
				if ( strcmp( ifa->ifa_name, ifname )==0 ) {
					found = 1;
					break;
				}
			}
		}
		if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);
		return found;
	}

#elif defined(USE_WINSOCK2)

#if 0
	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
		PIP_INTERFACE_INFO pInfo = NULL;
		ULONG ulOutBufLen = 0;

		DWORD dwRetVal = 0;
		int iReturn = 1;

		int i;

		dwRetVal = GetInterfaceInfo(NULL, &ulOutBufLen);
		if (dwRetVal == ERROR_INSUFFICIENT_BUFFER) {
			pInfo = (IP_INTERFACE_INFO *) MALLOC(ulOutBufLen);
			if (pInfo == NULL) {
				printf("Unable to allocate memory needed to call GetInterfaceInfo\n");
				return 1;
			}
		}

		dwRetVal = GetInterfaceInfo(pInfo, &ulOutBufLen);
		if (dwRetVal == NO_ERROR) {
			printf("Number of Adapters: %ld\n\n", pInfo->NumAdapters);
			for (i = 0; i < pInfo->NumAdapters; i++) {
	            printf("Adapter Index[%d]: %ld\n", i, pInfo->Adapter[i].Index);
				printf("Adapter Name[%d]: %ws\n\n", i, pInfo->Adapter[i].Name);
			}
			iReturn = 0;
		} else if (dwRetVal == ERROR_NO_DATA) {
	        printf("There are no network adapters with IPv4 enabled on the local system\n");
			iReturn = 0;
		} else {
	        printf("GetInterfaceInfo failed with error: %d\n", dwRetVal);
			iReturn = 1;
		}

		FREE(pInfo);

		return iReturn;
	}
#endif

	/**
	 * @ref http://stackoverflow.com/questions/1673931/how-do-i-enumerate-network-adapters-and-get-their-mac-addresses-in-win32-api-c
	 * @ref https://msdn.microsoft.com/en-us/library/windows/desktop/aa365917%28v=vs.85%29.aspx
	 */
	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
		ULONG outBufLen = 0;
		DWORD dwRetVal = 0;
		IP_ADAPTER_INFO* pAdapterInfos = (IP_ADAPTER_INFO*) malloc(sizeof(IP_ADAPTER_INFO));

		// retry up to 5 times, to get the adapter infos needed
		for( int i = 0; i < 5 && (dwRetVal == ERROR_BUFFER_OVERFLOW || dwRetVal == NO_ERROR); ++i )
		{
			dwRetVal = GetAdaptersInfo(pAdapterInfos, &outBufLen);
			if( dwRetVal == NO_ERROR )
			{
				break;
			}
			else if( dwRetVal == ERROR_BUFFER_OVERFLOW )
			{
				free(pAdapterInfos);
				pAdapterInfos = (IP_ADAPTER_INFO*) malloc(outBufLen);
			}
			else
			{
				pAdapterInfos = 0;
				break;
			}
		}
		if( dwRetVal == NO_ERROR )
		{
			IP_ADAPTER_INFO* pAdapterInfo = pAdapterInfos;
			while( pAdapterInfo )
			{
				IP_ADDR_STRING* pIpAddress = &(pAdapterInfo->IpAddressList);
				while( pIpAddress != 0 )
				{
					// 
					// <<<<
					// here pAdapterInfo->Address should contain the MAC address
					// >>>>
					// 

					printf("[%s] ip address: %s\n", pAdapterInfo->AdapterName, pAdapterInfo->IpAddressList.IpAddress.String);

					pIpAddress = pIpAddress->Next;
				}
				pAdapterInfo = pAdapterInfo->Next;
			}
		}
		free(pAdapterInfos);
		return false;
	}

#endif
		
	string Network::getIPAddress(const string & iface) {
		return getIPAddress(iface.c_str());
	}

	string Network::getIPAddress(const char * iface) {
		char ipaddr[1024] = {0,};
		if (s_get_ip_address(iface, ipaddr) > 0) {
			return string(ipaddr);
		}

		return "0.0.0.0";
	}


	/* select */
	Selector::Selector() : maxfds(0) {
		FD_ZERO(&readfds);
		FD_ZERO(&curfds);
	}
	
	Selector::~Selector() {
	}
	void Selector::set(int fd) {
		if (fd > maxfds) {
			maxfds = fd;
		}
		FD_SET(fd, &readfds);
	}
	void Selector::unset(int fd) {
		FD_CLR(fd, &readfds);
	}
	int Selector::select(unsigned long timeout_milli) {

		struct timeval timeout;
		
		curfds = readfds;
		timeout.tv_sec = timeout_milli / 1000;
		timeout.tv_usec = (timeout_milli % 1000) * 1000;
		
		return ::select(maxfds + 1, &curfds, NULL, NULL, &timeout);
	}
	vector<int> & Selector::getSelected() {
		selected.clear();
		for (int i = 0; i < maxfds + 1; i++) {
			if (FD_ISSET(i, &curfds)) {
				selected.push_back(i);
			}
		}
		return selected;
	}

	/* SOCKET */
#if defined(USE_BSD_SOCKET)

	class BsdSocket : public Socket {
	private:
		int sock;
	public:
		BsdSocket(int sock) : Socket(NULL, NULL, 0) {
			this->sock = sock;
		}
		BsdSocket(const char * host, int port) : Socket(NULL, host, port) {
		}
		virtual ~BsdSocket() {
		}
		virtual int connect() {

			int ret;
			struct addrinfo hints, * res;
			char port[10] = {0,};

			sock = -1;
			snprintf(port, sizeof(port), "%d", getPort());

			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_STREAM;
			ret = ::getaddrinfo(getHost(), port, &hints, &res);
			if (ret < 0) {
				perror("getaddrinfo() error");
				return -1;
			}

			sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock < 0) {
				perror("socket() error");
				freeaddrinfo(res);
				return -1;
			}

			ret = ::connect(sock, res->ai_addr, res->ai_addrlen);
			if (ret < 0) {
				freeaddrinfo(res);
				::close(sock);
				perror("connect() failed");
				return -1;
			}

			freeaddrinfo(res);
			
			return 0;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set(sock);
		}
		virtual bool compareFd(int fd) {
			return sock == fd;
		}
		virtual int getFd() {
			return sock;
		}
		virtual int recv(char * buffer, int max) {
			return ::read(sock, buffer, max);
		}
		virtual int send(char * buffer, int length) {
			return ::write(sock, buffer, length);
		}
		virtual void shutdown(/* type */) {}
		virtual void close() {
			::close(sock);
		}
	};

#elif defined(USE_WINSOCK2)

	/*
	 * Winsock2Socket
	 * reference
	 * - winsock: http://www.joinc.co.kr/modules/moniwiki/wiki.php/Site/win_network_prog/doc/winsock_basic
	 * - MSDN: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms737889%28v=vs.85%29.aspx
	 */
	class Winsock2Socket : public Socket {
	private:
		SOCKET sock;
		struct addrinfo * targetAddr;

	protected:
		
	public:
		Winsock2Socket(SOCKET sock) : Socket(NULL, NULL, 0), targetAddr(NULL) {
			this->sock = sock;
		}
		Winsock2Socket(const char * host, int port) : Socket(NULL, host, port), sock(INVALID_SOCKET), targetAddr(NULL) {
			WSADATA wsaData;
			struct addrinfo hints;
			
			char portStr[10] = {0,};
			int ret = WSAStartup(MAKEWORD(2,2), &wsaData);
			if (ret != 0) {
				std::cout << "WSAStartup error" << std::endl;
				// error
				return;
			}

			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;

			snprintf(portStr, sizeof(portStr), "%d", port);
			ret = getaddrinfo(host, portStr, &hints, &targetAddr);
			if (ret != 0) {
				std::cout << "getaddrinfo error" << std::endl;
				// error
				WSACleanup();
				targetAddr = NULL;
				return;
			}
		}

		virtual ~Winsock2Socket() {
			if (targetAddr) {
				freeaddrinfo(targetAddr);
			}
			WSACleanup();
		}

		virtual int connect() {

			struct addrinfo * addr = NULL;
			int ret;

			if (!targetAddr) {
				// error
				return -1;
			}

			for (addr = targetAddr; addr; addr = addr->ai_next) {
				// try to connect until one succeeds
				sock = ::socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
				if (sock == INVALID_SOCKET) {
					std::cout << "invalid socket" << std::endl;
					// error
					freeaddrinfo(targetAddr);
					WSACleanup();
					return -1;
				}

				ret = ::connect(sock, addr->ai_addr, (int)addr->ai_addrlen);
				if (ret == SOCKET_ERROR) {
					std::cout << "socket error" << std::endl;
					// error
					// freeaddrinfo(targetAddr);
					::closesocket(sock);
					sock = INVALID_SOCKET;
					continue;
				}
				break;
			}

			freeaddrinfo(targetAddr);
			targetAddr = NULL;

			if (sock == INVALID_SOCKET) {
				// error
				WSACleanup();
				return -1;
			}

			return 0;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set((int)sock);
		}
		virtual bool compareFd(int fd) {
			return (int)sock == fd;
		}
		virtual int getFd() {
			return (int)sock;
		}
		virtual int recv(char * buffer, int max) {
			if (sock == INVALID_SOCKET) {
				std::cout << "recv: invliad socket" << std::endl;
				return -1;
			}
			return ::recv(sock, buffer, max, 0);
		}

		virtual int send(char * buffer, int length) {
			if (sock == INVALID_SOCKET) {
				std::cout << "recv: invliad socket" << std::endl;
				return -1;
			}
			return ::send(sock, buffer, length, 0);
		}

		virtual void shutdown(/* type */) {
		}

		virtual void close() {
			if (sock == INVALID_SOCKET) {
				std::cout << "recv: invliad socket" << std::endl;
				return;
			}
			closesocket(sock);
		}
	};

#endif

	/*
	 * Socket
	 */

	 Socket::Socket(Socket * impl, const char * host, int port) : socketImpl(impl), port(port) {

		 memset(this->host, 0, sizeof(this->host));

		if (host) {
			snprintf(this->host, sizeof(this->host), "%s", host);
		}

	 }

	Socket::Socket(const char * host, int port) : port(port) {

		memset(this->host, 0, sizeof(this->host));

		if (host) {
			snprintf(this->host, sizeof(this->host), "%s", host);
		}

#if defined(USE_BSD_SOCKET)
		socketImpl = new BsdSocket(host, port);
#elif defined(USE_WINSOCK2)
		socketImpl = new Winsock2Socket(host, port);
#endif
	}

	Socket::~Socket() {
		if (socketImpl) {
			delete socketImpl;
		}
	}

	int Socket::connect() {
		return socketImpl ? socketImpl->connect() : -1;
	}

	void Socket::registerSelector(Selector & selector) {
		if (socketImpl) {
			socketImpl->registerSelector(selector);
		}
	}
	bool Socket::compareFd(int fd) {
		return socketImpl ? socketImpl->compareFd(fd) : false;
	}
	int Socket::getFd() {
		return socketImpl ? socketImpl->getFd() : -1;
	}
	int Socket::recv(char * buffer, int max) {
		return socketImpl ? socketImpl->recv(buffer, max) : -1;
	}
	
	int Socket::send(char * buffer, int length) {
		return socketImpl ? socketImpl->send(buffer, length) : -1;
	}

	void Socket::shutdown(/* type */) {
		if (socketImpl) {
			socketImpl->shutdown();
		}
	}
	
	void Socket::close() {
		if (socketImpl) {
			socketImpl->close();
		}
	}

	char * Socket::getHost() {
		return host;
	}

	int Socket::getPort() {
		return port;
	}
	
#if defined(USE_BSD_SOCKET)
	/*
	 * BsdServerSocket
	 * http://www.joinc.co.kr/modules/moniwiki/wiki.php/Site/Network_Programing/Documents/socket_beginning
	 */
	class BsdServerSocket : public ServerSocket{
	private:
		int sock;
	public:
		BsdServerSocket(int port) : ServerSocket(NULL, port) {
			sock = ::socket(AF_INET, SOCK_STREAM, 0);
			if (sock < 0) {
				// error
			}
		}
		
		virtual ~BsdServerSocket() {
			close();
		}

		virtual void setReuseAddr() {
			int status;
			int on = 1;
			status = ::setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on));
			if (status != 0) {
				// error
				::close(sock);
			}
		}

		virtual void registerSelector(Selector & selector) {
			selector.set(sock);
		}
		virtual bool compareFd(int fd) {
			return sock == fd;
		}
		virtual int getFd() {
			return sock;
		}

		virtual bool bind() {
			struct sockaddr_in addr;

			memset(&addr, 0, sizeof(addr));
			addr.sin_family = AF_INET;
			addr.sin_addr.s_addr = htonl(INADDR_ANY);
			addr.sin_port = htons(getPort());
			if (::bind(sock, (struct sockaddr*)&addr, sizeof(addr)) != 0) {
				// error
				::close(sock);
				return false;
			}
			return true;
		}
	
		virtual bool listen(int max) {
			if (::listen(sock, max) != 0) {
				// error
				::close(sock);
				return false;
			}
			return true;
		}
	
		virtual Socket * accept() {
			struct sockaddr_in clientaddr;
			socklen_t clientaddr_len = 0;
			int client = ::accept(sock, (struct sockaddr*)&clientaddr, &clientaddr_len);
			if (client < 0) {
				// error
				return NULL;
			}

			return new BsdSocket(client);
		}

		virtual void close() {
			::close(sock);
		}
	};
#elif defined(USE_WINSOCK2)

	class Winsock2ServerSocket : public ServerSocket {
	private:
		SOCKET sock;
		struct addrinfo * addr;

	protected:

	public:
		Winsock2ServerSocket(int port) : ServerSocket(NULL, port), addr(NULL) {
			WSADATA wsaData;
			struct addrinfo hints;
			char portStr[10] = {0,};
						
			int ret = WSAStartup(MAKEWORD(2,2), &wsaData);
			if (ret != 0) {
				// error
				return;
			}

			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_INET;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;
			hints.ai_flags = AI_PASSIVE;

			snprintf(portStr, sizeof(portStr), "%d", port);

			// Resolve the server address and port
			ret = getaddrinfo(NULL, portStr, &hints, &addr);
			if (ret != 0) {
				// error
				WSACleanup();
				return;
			}

			sock = ::socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
			if (sock == SOCKET_ERROR) {
				// error
				freeaddrinfo(addr);
				WSACleanup();
				return;
			}
		}

		virtual ~Winsock2ServerSocket() {
			close();
			WSACleanup();
		}

		virtual void setReuseAddr() {
			// TODO: implement - https://msdn.microsoft.com/en-us/library/windows/desktop/ms740621%28v=vs.85%29.aspx
		}

		virtual void registerSelector(Selector & selector) {
			selector.set((int)sock);
		}
		virtual bool compareFd(int fd) {
			return (int)sock == fd;
		}
		virtual int getFd() {
			return (int)sock;
		}

		virtual bool bind() {

			int ret;

			ret = ::bind(sock, addr->ai_addr, (int)addr->ai_addrlen);
			if (ret == SOCKET_ERROR) {
				// error
				freeaddrinfo(addr);
				closesocket(sock);
				WSACleanup();
				return false;
			}

			freeaddrinfo(addr);

			return true;
		}
	
		virtual bool listen(int max) {

			int ret = ::listen(sock, max); // SOMAXCONN
			if (ret == SOCKET_ERROR) {
				// error ; WSAGetLastError();
				closesocket(sock);
				WSACleanup();
				return false;
			}

			return true;
		}
	
		virtual Socket * accept() {
			SOCKET client = ::accept(sock, (sockaddr*)NULL, (int*)NULL);
			if (client == INVALID_SOCKET) {
				// error
				return NULL;
			}

			return new Winsock2Socket(client);
		}

		virtual void close() {
			closesocket(sock);
		}
	};

#endif

	/*
	 * ServerSocket
	 */
	ServerSocket::ServerSocket(ServerSocket * impl, int port) : serverSocketImpl(impl), port(port) {
	}

	ServerSocket::ServerSocket(int port) : port(port) {

#if defined(USE_BSD_SOCKET)
		serverSocketImpl = new BsdServerSocket(port);
#elif defined(USE_WINSOCK2)
		serverSocketImpl = new Winsock2ServerSocket(port);
#endif

	}

	ServerSocket::~ServerSocket() {
		if (serverSocketImpl) {
			delete serverSocketImpl;
		}
	}

	void ServerSocket::setReuseAddr() {
		if (serverSocketImpl) {
			serverSocketImpl->setReuseAddr();
		}
	}

	void ServerSocket::registerSelector(Selector & selector) {
		if (serverSocketImpl) {
			serverSocketImpl->registerSelector(selector);
		}
	}

	bool ServerSocket::compareFd(int fd) {
		return serverSocketImpl ? serverSocketImpl->compareFd(fd) : false;
	}

	int ServerSocket::getFd() {
		return serverSocketImpl ? serverSocketImpl->getFd() : -1;
	}
	
	bool ServerSocket::bind() {
		return serverSocketImpl ? serverSocketImpl->bind() : false;
	}
	
	bool ServerSocket::listen(int max) {
		return serverSocketImpl ? serverSocketImpl->listen(max) : false;
	}
	
	Socket * ServerSocket::accept() {
		return serverSocketImpl ? serverSocketImpl->accept() : NULL;
	}

	void ServerSocket::close() {
		if (serverSocketImpl) {
			serverSocketImpl->close();
		}
	}

	int ServerSocket::getPort() {
		return port;
	}

	
	// date
	
#if defined(USE_UNIX_STD)
	static string s_date_format(string fmt, TIME time) {
		char buffer[1024] = {0,};
		strftime(buffer, sizeof(buffer), fmt.c_str(), gmtime((time_t *)&time));

		return string(buffer);
	}
#elif defined(USE_MS_WIN)
	static string s_date_format(string fmt, TIME time) {
		char buffer[1024] = {0,};
		SYSTEMTIME stUTC;
		FileTimeToSystemTime(&time, &stUTC);
		snprintf(buffer, sizeof(buffer), "%d-%02d-%02d %02d:%02d:%02d", stUTC.wYear, stUTC.wMonth, stUTC.wDay, stUTC.wHour, stUTC.wMinute, stUTC.wSecond);
		return string(buffer);
	}
#else
	// sleep
#endif

	string Date::DEFAULT_FORMAT = "%Y-%m-%d %H:%M:%S";

	/**
	 * @brief seconds to string
	 * @ref http://stackoverflow.com/questions/10446526/get-last-modified-time-of-file-in-linux
	 */
	string Date::format(string fmt, TIME seconds) {
		return s_date_format(fmt, seconds);
	}
	

	// file system
#if defined(USE_UNIX_STD)

	static bool s_is_separator(char c) {
		return (c == '/');
	}
	static string s_remove_if_last(string & path, char m) {
		SUPPRESS_WARNING(m);
		if (!path.empty()
			&& path.length() > 1
			&& s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
	static bool s_is_root_path(string & path) {
		return !path.compare("/");
	}
	static bool s_exists(string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c
		struct stat st;
		return (stat(path.c_str(), &st) == 0);
	}
	static bool s_is_file(string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/3536781/accessing-directories-in-c/3536853#3536853
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? false : true);
	}
	static bool s_is_directory(string & path) {

		if (path.empty()) {
			return false;
		}
		
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? true : false);
	}
	static bool s_is_writable(string & path) {
		return (access(path.c_str(), W_OK) == 0);
	}
	static string s_get_parent_path(string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

		string p = s_remove_if_last(path, '/');
		size_t f = p.find_last_of("/");
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_if_last(path, '/');
		}

		size_t f = path.find_last_of("/");
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		size_t f = path.find_last_of("/");
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
			string p = s_remove_if_last(path, '/');
			size_t f = p.find_last_of("/");
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(string & path) {
		size_t f = path.find_last_of(".");
		if (f == string::npos) {
			return "";
		}
		return path.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, mode_t mode) {
	
		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(tmp[len - 1] == '/') {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(*p == '/') {
				*p = 0;
				mkdir(tmp, mode); // ignore error (just try)
				*p = '/';
			}
		}
	
		return mkdir(tmp, mode);
	}

	static TIME s_get_creation_date(string path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return (long int)st.st_ctime;
	}

	static TIME s_get_modified_date(string path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return (long int)st.st_mtime;
	}
	
#elif defined(USE_MS_WIN)

	static bool s_is_separator(char c);
	static string s_remove_if_last(string & path, char m);
	static bool s_is_fullpath(string & path);
	static bool s_is_root_path(string & path);
	static bool s_exists(string & path);
	static bool s_is_file(string & path);
	static bool s_is_directory(string & path);
	static string s_get_parent_path(string & path);
	static string s_get_path_part(string & path);
	static string s_get_filename_part(string & path);
	static string s_get_entity_name_part(string & path);
	static string s_get_ext(string & path);
	static int s_mkdir(const char *dir, int mode);
	static TIME s_get_creation_date(string path);
	static TIME s_get_modified_date(string path);


	static bool s_is_separator(char c) {
		return (c == '/' || c == '\\');
	}
	static string s_remove_if_last(string & path, char m) {
		if (!path.empty()
			&& path.length() > 1
			&& s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
	static bool s_is_root_path(string & path) {
		return !path.compare("/") || !path.compare("\\");
	}
	static bool s_exists(string & path) {

		if (path.empty()) {
			return false;
		}

		if (s_is_directory(path) || s_is_file(path)) {
			return true;
		}
		
		return false;
	}
	static bool s_is_file(string & path) {

		if (path.empty()) {
			return false;
		}
		
		//// http://stackoverflow.com/questions/146924/how-can-i-tell-if-a-given-path-is-a-directory-or-a-file-c-c
		//if (GetFileAttributes(path.c_str()) == FILE_ATTRIBUTE_NORMAL) {
		//	return true;
		//}
		//return false;

		// http://stackoverflow.com/questions/146924/how-can-i-tell-if-a-given-path-is-a-directory-or-a-file-c-c
		struct stat s;
		if (stat(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFREG ? true : false);
	}
	static bool s_is_directory(string & path) {

		if (path.empty()) {
			return false;
		}
		
		//// http://stackoverflow.com/questions/146924/how-can-i-tell-if-a-given-path-is-a-directory-or-a-file-c-c
		//if (GetFileAttributes(path.c_str()) == FILE_ATTRIBUTE_DIRECTORY) {
		//	return true;
		//}

		// http://stackoverflow.com/questions/146924/how-can-i-tell-if-a-given-path-is-a-directory-or-a-file-c-c
		struct stat s;
		if (stat(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFDIR ? true : false);
	}
	static bool s_is_writable(string & path) {
		if (!s_exists(path)) {
			return false;
		}
		return (_access(path.c_str(), 2) == 0);
	}
	static string s_get_parent_path(string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

		string p = s_remove_if_last(path, '\\');
		int f = p.find_last_of("\\");
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_if_last(path, '\\');
		}

		int f = path.find_last_of("\\");
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		int f = path.find_last_of("\\");
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
			string p = s_remove_if_last(path, '\\');
			int f = p.find_last_of("\\");
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(string & path) {
		int f = path.find_last_of(".");
		if (f == string::npos) {
			return "";
		}
		return path.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, int mode) {
	
		// https://msdn.microsoft.com/en-us/library/2fkk4dzw.aspx

		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(tmp[len - 1] == '\\') {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(*p == '\\') {
				*p = 0;
				_mkdir(tmp);
				*p = '\\';
			}
		}
	
		return _mkdir(tmp);


		return 0;
	}
	static TIME s_get_creation_date(string path) {

		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return ftCreate;
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return ftCreate;
		}

		return ftCreate;
	}
	static TIME s_get_modified_date(string path) {
		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return ftWrite;
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return ftWrite;
		}

		return ftWrite;
	}
#endif

	/**
	 * @brief file constructor
	 */
	File::File() {
	}

	File::~File() {
	}

	bool File::isRootPath(string path){
		return s_is_root_path(path);
	}

	bool File::isFullpath(string path) {
		return s_is_fullpath(path);
	}
	
	bool File::exists(string path){
		return s_exists(path);
	}

	bool File::isFile(string path){
		return s_is_file(path);
	}

	bool File::isDirectory(string path){
		return s_is_directory(path);
	}

	bool File::isWritable(string path) {
		return s_is_writable(path);
	}

	string File::getParentPath(string path) {
		return s_get_parent_path(path);
	}

	string File::getPathPart(string path){
		return s_get_path_part(path);
	}

	string File::getFileNamePart(string path){
		return s_get_filename_part(path);
	}

	string File::getExtension(string path){
		return s_get_ext(path);
	}

	string File::getEntityNamePart(string path) {
		return s_get_entity_name_part(path);
	}

	bool File::compareExtension(string path, string extension){
		
		string a = s_get_ext(path);
		string b = s_get_ext(extension);

		if (a.empty() || b.empty()) {
			return false;
		}

		return (!a.compare(b));
	}

	int File::mkdir(string path) {
		return s_mkdir(path.c_str(), 0755);
	}

	string File::fullpath(string dir, string filename) {
		
		if (dir.length() > 0 && *dir.rbegin() != '/') {
			dir += "/";
		}

		if (filename.length() > 0) {
			size_t f = filename.find_first_not_of('/');
			if (f != string::npos) {
				filename = filename.substr(f);
			}
		}

		return dir + filename;
	}

	string File::getCreationDate(string path, string fmt) {
		TIME t = s_get_creation_date(path);
		return Date::format(fmt, t);
	}
	
	string File::getModifiedDate(string path, string fmt) {
		TIME t = s_get_modified_date(path);
		return Date::format(fmt, t);
	}
	
} /* OS */

