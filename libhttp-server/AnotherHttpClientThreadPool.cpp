#include "AnotherHttpClientThreadPool.hpp"

#include "ChunkedTransfer.hpp"
#include "FixedTransfer.hpp"

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
    void AnotherHttpClientThread::setUserData(UTIL::AutoRef<UserData> userData) {
        this->userData = userData;
    }
    void AnotherHttpClientThread::setRequest(const Url & url, const string & method, const LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer) {
        httpClient.setUrl(url);
        httpClient.setRequest(method, additionalHeaderFields, transfer);
        httpClient.setFollowRedirect(true);
        setFlag(true);
    }
    void AnotherHttpClientThread::run() {
        
        while (!interrupted()) {
            
            if (!flagged()) {
                idle(10);
                continue;
            }
            
            httpClient.execute();
			userData = NULL;
            
            setFlag(false);
        }
    }
    void AnotherHttpClientThread::onResponseHeader(HttpResponse & response) {
       
        if (response.getHeader().isChunkedTransfer()) {
            response.setTransfer(AutoRef<DataTransfer>(new ChunkedTransfer));
        } else if (response.getHeader().getContentLength() > 0) {
            response.setTransfer(AutoRef<DataTransfer>(new FixedTransfer(response.getHeader().getContentLength())));
        } else {
            // do nothing
        }
    }
    void AnotherHttpClientThread::onTransferDone(DataTransfer * transfer) {
        string content;
        if (transfer) {
            content = transfer->getString();
        }
        
        if (listener) {
            listener->onRequestComplete(httpClient.getUrl(), httpClient.getResponse(), content, &userData);
        }
    }
    void AnotherHttpClientThread::onError() {
        if (listener) {
            listener->onRequestError(httpClient.getUrl(), &userData);
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
    
    void AnotherHttpClientThreadPool::setRequest(const Url & url, const std::string & method, UTIL::AutoRef<DataTransfer> transfer, UTIL::AutoRef<UserData> userData) {
        
        setRequest(url, method, LinkedStringMap(), transfer, userData);
    }

	void AnotherHttpClientThreadPool::setRequest(const Url & url, const string & method, const map<string, string> & additionalHeaderFields, AutoRef<DataTransfer> transfer, AutoRef<UserData> userData) {

		LinkedStringMap lst;
		for (map<string, string>::const_iterator iter = additionalHeaderFields.begin(); iter != additionalHeaderFields.end(); iter++) {
			lst[iter->first] = iter->second;
		}

		setRequest(url, method, lst, transfer, userData);
	}
    
    void AnotherHttpClientThreadPool::setRequest(const Url & url, const string & method, const UTIL::LinkedStringMap & additionalHeaderFields, AutoRef<DataTransfer> transfer, AutoRef<UserData> userData) {
        AnotherHttpClientThread * thread = (AnotherHttpClientThread *)acquire();
        if (thread) {
            thread->setOnRequestCompleteListener(listener);
            thread->setUserData(userData);
            thread->setRequest(url, method, additionalHeaderFields, transfer);
            enqueue(thread);
        }
    }
 
    void AnotherHttpClientThreadPool::setOnRequestCompleteListener(OnRequestCompleteListener * listener) {
        this->listener = listener;
    }
}