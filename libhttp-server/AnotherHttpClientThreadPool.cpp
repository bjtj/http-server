#include "AnotherHttpClientThreadPool.hpp"

#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"
#include "StringDataSink.hpp"

namespace HTTP {

    using namespace std;
    using namespace OS;
    using namespace UTIL;
   
    /**
     * @brief
     */
    
    AnotherHttpClientThread::AnotherHttpClientThread() : FlaggableThread(false), listener(NULL) {
        httpClient.setOnResponseListener(this);
    }
    AnotherHttpClientThread::~AnotherHttpClientThread() {
    }
	AutoRef<DataSink> AnotherHttpClientThread::getDataSink() {
		return AutoRef<DataSink>(new StringDataSink);
	}
    void AnotherHttpClientThread::setUserData(UTIL::AutoRef<UserData> userData) {
        this->userData = userData;
    }
    void AnotherHttpClientThread::setRequestWithFixedTransfer(const Url & url, const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer, size_t size) {
        httpClient.setUrl(url);
        httpClient.setRequestWithFixedTransfer(method, additionalHeaderFields, transfer, size);
        httpClient.setFollowRedirect(true);
        httpClient.setUserData(userData);
        userData = NULL;
        setFlag(true);
    }
	void AnotherHttpClientThread::setRequestWithChunkedTransfer(const Url & url, const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer) {
        httpClient.setUrl(url);
        httpClient.setRequestWithChunkedTransfer(method, additionalHeaderFields, transfer);
        httpClient.setFollowRedirect(true);
        httpClient.setUserData(userData);
        userData = NULL;
        setFlag(true);
    }
    void AnotherHttpClientThread::run() {
        
        while (!interrupted()) {
            
            if (!flagged()) {
                idle(10);
                continue;
            }
            
            httpClient.execute();
            
            setFlag(false);
        }
    }
    void AnotherHttpClientThread::onTransferDone(HttpResponse & response, AutoRef<DataSink> sink, AutoRef<UserData> userData) {
        string content;
        if (!sink.nil()) {
            content = ((StringDataSink*)&sink)->data();
        }
        
        if (listener) {
            listener->onRequestComplete(httpClient.getUrl(), httpClient.getResponse(), content, &userData);
        }
    }
    void AnotherHttpClientThread::onError(Exception & e, AutoRef<UserData> userData) {
        if (listener) {
            listener->onRequestError(e, httpClient.getUrl(), &userData);
        }
    }
    
    void AnotherHttpClientThread::setOnRequestCompleteListener(OnRequestCompleteListener * listener) {
        this->listener = listener;
    }
    
    
    /**
     * @brief
     */
    
    class AnotherHttpClientThreadInstanceCreator : public InstanceCreator<FlaggableThread*> {
    private:
    public:
        AnotherHttpClientThreadInstanceCreator() {}
        virtual ~AnotherHttpClientThreadInstanceCreator() {}
        virtual UTIL::FlaggableThread * createInstance() {
            return new AnotherHttpClientThread;
        }
        virtual void releaseInstance(UTIL::FlaggableThread * inst) {
            delete inst;
        }
    };
    
    static AnotherHttpClientThreadInstanceCreator instanceCreator;
 
    /**
     * @brief
     */
    
    AnotherHttpClientThreadPool::AnotherHttpClientThreadPool(size_t maxThreads) : ThreadPool(maxThreads, instanceCreator), listener(NULL) {
    }
    AnotherHttpClientThreadPool::~AnotherHttpClientThreadPool() {
    }
    
    void AnotherHttpClientThreadPool::setRequestWithFixedTransfer(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, size_t size, UTIL::AutoRef<UserData> userData) {
        
        setRequestWithFixedTransfer(url, method, LinkedStringMap(), transfer, size, userData);
    }

	void AnotherHttpClientThreadPool::setRequestWithChunkedTransfer(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData) {
        
        setRequestWithChunkedTransfer(url, method, LinkedStringMap(), transfer, userData);
    }

	void AnotherHttpClientThreadPool::setRequestWithFixedTransfer(const Url & url, const string & method, const map<string, string> & additionalHeaderFields, AutoRef<DataTransfer> transfer, size_t size, AutoRef<UserData> userData) {

		LinkedStringMap lst;
		for (map<string, string>::const_iterator iter = additionalHeaderFields.begin(); iter != additionalHeaderFields.end(); iter++) {
			lst[iter->first] = iter->second;
		}

		setRequestWithFixedTransfer(url, method, lst, transfer, size, userData);
	}

	void AnotherHttpClientThreadPool::setRequestWithChunkedTransfer(const Url & url, const string & method, const map<string, string> & additionalHeaderFields, AutoRef<DataTransfer> transfer, AutoRef<UserData> userData) {

		LinkedStringMap lst;
		for (map<string, string>::const_iterator iter = additionalHeaderFields.begin(); iter != additionalHeaderFields.end(); iter++) {
			lst[iter->first] = iter->second;
		}

		setRequestWithChunkedTransfer(url, method, lst, transfer, userData);
	}
    
    void AnotherHttpClientThreadPool::setRequestWithFixedTransfer(const Url & url, const string & method, const UTIL::LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer, size_t size, AutoRef<UserData> userData) {
        AnotherHttpClientThread * thread = (AnotherHttpClientThread *)acquire();
        if (thread) {
            thread->setOnRequestCompleteListener(listener);
            thread->setUserData(userData);
            thread->setRequestWithFixedTransfer(url, method, additionalHeaderFields, transfer, size);
            enqueue(thread);
        }
    }

	void AnotherHttpClientThreadPool::setRequestWithChunkedTransfer(const Url & url, const string & method, const UTIL::LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer, AutoRef<UserData> userData) {
        AnotherHttpClientThread * thread = (AnotherHttpClientThread *)acquire();
        if (thread) {
            thread->setOnRequestCompleteListener(listener);
            thread->setUserData(userData);
            thread->setRequestWithChunkedTransfer(url, method, additionalHeaderFields, transfer);
            enqueue(thread);
        }
    }
 
    void AnotherHttpClientThreadPool::setOnRequestCompleteListener(OnRequestCompleteListener * listener) {
        this->listener = listener;
    }
}
