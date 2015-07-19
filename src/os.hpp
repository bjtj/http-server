#ifndef __OS_HPP__
#define __OS_HPP__

/*
 * @brief standard header
 */
#include <string>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>

/**
 * @brief common feature
 * unused : http://stackoverflow.com/questions/3599160/unused-parameter-warnings-in-c-code
 */
#define SUPPRESS_WARNING(x) (void)(x)

/*
* Predefined OS macros
* ====================
* http://stackoverflow.com/questions/142508/how-do-i-check-os-with-a-preprocessor-directive
*/

#if defined(__APPLE__) || defined(__MACH__) /* mac os x */

#	define USE_UNIX_STD
#	define USE_APPLE_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>

#	define TIME long int

#elif defined(unix) || defined(__unix__) || defined(__unix) /* unix or linux */

#	define USE_UNIX_STD
#	define USE_POSIX_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET
#	define USE_PRCTL

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>

#	define TIME long int

#elif defined(_WIN32) || defined(_WIN64) /* windows */

#	define USE_MS_WIN
#	define USE_WIN_THREAD
#	define USE_WINSOCK2

#	define snprintf _snprintf_s

#	define TIME FILETIME

#	include <direct.h>
#	include <sys/stat.h>
#	include <io.h>

#endif /* OS DETECTING */



/*
 * Semaphore
 */

#if defined(USE_APPLE_SEMAPHORE)

#	include <dispatch/dispatch.h>
typedef dispatch_semaphore_t SEM_HANDLE;

#elif defined(USE_POSIX_SEMAPHORE)

#	include <semaphore.h>
typedef sem_t SEM_HANDLE;

#else

#	define SEM_HANDLE HANDLE // TODO: windows how to?

#endif

/*
 * Thread
 */
#if defined(USE_PTHREAD)

#	include <pthread.h>

#if defined(USE_PRCTL)
#	include <sys/prctl.h>
#endif

typedef pthread_t THREAD_HANDLE;

#elif defined(USE_WIN_THREAD)

#	define WIN32_LEAN_AND_MEAN
#	define _WINSOCKAPI_ /* http://stackoverflow.com/questions/1372480/c-redefinition-header-files-winsock2-h */
#	include <Windows.h>
#	include <process.h>
typedef HANDLE THREAD_HANDLE;

#endif

/*
 * @brief Socket
 */
#if defined(USE_BSD_SOCKET) /* BSD */


#	include <arpa/inet.h>
#	include <sys/types.h>
#	include <sys/stat.h>
#	include <sys/socket.h>
#	include <ifaddrs.h>

#	include <netdb.h>
#	include <netinet/in.h>

#elif defined(USE_WINSOCK2) /* winsock2 */

// reference: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms737629%28v=vs.85%29.aspx
// refenrece: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365947%28v=vs.85%29.aspx

#	include <WinSock2.h>
#	include <WS2tcpip.h>
#	include <ws2ipdef.h>
#	include <iphlpapi.h>

#	pragma comment(lib, "Ws2_32.lib")
#	pragma comment(lib, "iphlpapi.lib")

/* Note: could also use malloc() and free() */
#	define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x)) 
#	define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

#endif

namespace OS {

	/**
	 * @brief milli seconds sleep
	 */
	void idle(unsigned long timeout);

	/**
	 * @brief get tick count
	 */
	unsigned long tick_milli();

	/**
	 * @brief semaphore
	 */
	class Semaphore {
	private:
		SEM_HANDLE handle;
	public:
		Semaphore(int initial);
		virtual ~Semaphore();
		void wait();
		void post();
	};

	/*
	 * @brief Thread
	 */
	class Thread {
	private: /* private */
		
		THREAD_HANDLE handle;
		bool signal_interrupt;
		bool running;

	private: /* private static */
		
		static unsigned int s_thread_id_seed;

	public: /* public */
		
		unsigned int id;
		
	public: /* methods */
		
		Thread();
		virtual ~Thread();

		unsigned int getId();

		void reset();

		bool start();
		void interrupt();
		bool interrupted();
		bool isRunning();

		void join();

		virtual void run() = 0;
	};

	/**
	 * @brief network
	 */
	class Network {
	private:
	public:
		static std::string getIPAddress(const std::string & iface);
		static std::string getIPAddress(const char * iface);
	};

	/**
	 * @brief selector
	 */
	class Selector {
	private:
		int maxfds;
        fd_set readfds;
		fd_set curfds;
		std::vector<int> selected;
	public:
		Selector();
		virtual ~Selector();

		virtual void set(int fd);
		virtual void unset(int fd);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<int> & getSelected();
	};

	/**
	 * @brief Selectable interface
	 */
	class Selectable {
	public:
		Selectable() {}
		virtual ~Selectable() {}

		virtual void registerSelector(Selector & selector) = 0;
	};


	/*
	 * @brief Socket
	 */
	class Socket : public Selectable {
	private:
		Socket * socketImpl;
		char host[2048];
		int port;
	protected:
		Socket(Socket * impl, const char * host, int port);
	public:
		Socket(const char * host, int port);
		virtual ~Socket();

		virtual int connect();

		virtual void registerSelector(Selector & selector);
		virtual bool compareFd(int fd);
		virtual int getFd();

		virtual int recv(char * buffer, int max);
		virtual int send(char * buffer, int length);

		virtual void shutdown(/* type */);
		virtual void close();

		char * getHost();
		int getPort();
	};

	/*
	 * @brief Server Socket
	 */
	class ServerSocket : public Selectable {
	private:
		ServerSocket * serverSocketImpl;
		int port;
	protected:
		ServerSocket(ServerSocket * impl, int port);
	public:
		ServerSocket(int port);
		virtual ~ServerSocket();

		virtual void setReuseAddr();

		virtual void registerSelector(Selector & selector);
		virtual bool compareFd(int fd);
		virtual int getFd();

		virtual bool bind();
		virtual bool listen(int max);
		virtual Socket * accept();
		virtual void close();

		int getPort();
	};

	/**
	 * @brief date
	 */
	class Date {
	private:
	public:
		static std::string DEFAULT_FORMAT;
	public:
		static std::string format(std::string fmt, TIME seconds);
	};

	/**
	 * @brief file
	 */
	class File {
	private:
		std::string path;
	public:
		File();
		File(std::string path);
		virtual ~File();
		static bool isRootPath(std::string path);
		static bool isFullpath(std::string path);
		static bool exists(std::string path);
		static bool isFile(std::string path);
		static bool isDirectory(std::string path);
		static bool isWritable(std::string path);
		static std::string getParentPath(std::string path);
		static std::string getPathPart(std::string path);
		static std::string getFileNamePart(std::string path);
		static std::string getExtension(std::string path);
		static std::string getEntityNamePart(std::string path);
		static bool compareExtension(std::string path, std::string extension);
		static int mkdir(std::string path);
		static std::string fullpath(std::string dir, std::string filename);
		static std::string getCreationDate(std::string path, std::string fmt = Date::DEFAULT_FORMAT);
		static std::string getModifiedDate(std::string path, std::string fmt = Date::DEFAULT_FORMAT);

		static std::vector<File> list(std::string path);

		std::string getName();
		virtual std::string toString();
	};
	
}

#endif
