#include <libhttp-server/ConnectionManager.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/TaskThreadPool.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/CountDownLatch.hpp>
#include "Expect.hpp"

using namespace std;
using namespace osl;
using namespace http;


class EchoCommunication : public Communication {
private:
	string content;
public:
	EchoCommunication() {}
	virtual ~EchoCommunication() {}

	string toString(InetAddress addr) {
		return addr.getHost() + ":" + Text::toString(addr.getPort());
	}

	virtual void onConnected(AutoRef<Connection> connection) {
		cout << " ** connected from: " << toString(connection->getRemoteAddress()) << endl;
	}
	virtual bool onReceivable(AutoRef<Connection> connection) {
		Packet & packet = connection->read();
		content.append(packet.getData(), packet.getLength());
		return true;
	}
	virtual bool onWriteable(AutoRef<Connection> connection) {
		if (content.size() == 0) {
			return false;
		}
		connection->send(content.c_str(), content.size());
		content = "";
		return true;
	}
	virtual void onDisconnected(AutoRef<Connection> connection) {
		cout << " ** disconnected! - " << toString(connection->getRemoteAddress()) << endl;
	}
	virtual bool isCommunicationCompleted() {
		return false;
	}
};

class EchoCommunicationMaker : public CommunicationMaker {
public:
	EchoCommunicationMaker() {}
	virtual ~EchoCommunicationMaker() {}
	AutoRef<Communication> makeCommunication() {
		cout << " ** make communication" << endl;
		return AutoRef<Communication>(new EchoCommunication);
	}
};

class PollingThread : public Thread {
private:
	ConnectionManager * cm;
public:
	PollingThread(ConnectionManager * cm) : cm(cm) {}
	virtual ~PollingThread() {}
	virtual void run() {
		cout << " ** start polling" << endl;
		while (!interrupted()) {
			cm->poll(100);
		}
		cout << " ** stop polling" << endl;
	}
};



class BasicConnectionManagerTestCase : public TestCase {
private:
	ConnectionManager * cm;
	PollingThread * thread;
	int port;
public:
	BasicConnectionManagerTestCase() : TestCase("BasicConnectionManagerTestCase"), port(0) {}
	virtual ~BasicConnectionManagerTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		port = env.getIntegerProperty("listen.port");
		cm = new ConnectionManager(
			ConnectionConfig(AutoRef<CommunicationMaker>(new EchoCommunicationMaker), 5));
		cm->start(port);

		thread = new PollingThread(cm);
		thread->start();
	}
	virtual void tearDown() {

		thread->interrupt();
		thread->join();
		delete thread;
		
		cm->stop();
		delete cm;
	}
	virtual void test() {

		class NormalVisitorTask : public Task {
		private:
			Expect & done;
			InetAddress remoteAddr;
		public:
			NormalVisitorTask(Expect & done, InetAddress remoteAddr)
				: done(done), remoteAddr(remoteAddr) {}
			virtual ~NormalVisitorTask() {}
			virtual void onTask() {
				Socket sock(remoteAddr);
				sock.connect();
				sock.close();
				done--;
			}
		};
		
		TaskThreadPool pool(10);
		pool.start();

		Expect done(1);
		InetAddress addr = cm->getServerAddress();
		pool.setTask(AutoRef<Task>(new NormalVisitorTask(done, InetAddress("127.0.0.1", addr.getPort()))));
		done.wait();
		
		pool.stop();
	}
};

class ConnectionManagerTestCase : public TestCase {
private:
	ConnectionManager * cm;
	PollingThread * thread;
	int port;
public:
	ConnectionManagerTestCase() : TestCase("ConnectionManagerTestCase"), port(0) {}
	virtual ~ConnectionManagerTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		port = env.getIntegerProperty("listen.port");
		cm = new ConnectionManager(
			ConnectionConfig(
				AutoRef<CommunicationMaker>(new EchoCommunicationMaker),
				5));
		cm->start(port);

		thread = new PollingThread(cm);
		thread->start();
	}
	virtual void tearDown() {

		thread->interrupt();
		thread->join();
		delete thread;
		
		cm->stop();
		delete cm;
	}
	virtual void test() {

		class EchoVisitorTask : public Task {
		private:
			Expect & done;
			InetAddress remoteAddr;
		public:
			EchoVisitorTask(Expect & done, InetAddress remoteAddr) : done(done), remoteAddr(remoteAddr) {}
			virtual ~EchoVisitorTask() {}
			virtual void onTask() {
				Socket sock(remoteAddr);
				sock.connect();
				string say = "yeah!";
				sock.send(say.c_str(), say.size());
				char buffer[1024] = {0,};
				sock.recv(buffer, sizeof(buffer));
				sock.close();
				
				ASSERT(say, ==, buffer);
								
				done--;
			}
		};
		
		TaskThreadPool pool(10);
		pool.start();

		Expect done(1);

		InetAddress addr = cm->getServerAddress();
		pool.setTask(AutoRef<Task>(new EchoVisitorTask(done, InetAddress("127.0.0.1", addr.getPort()))));

		done.wait();
		
		pool.stop();
	}
};


class ConnectionManagerMultiConnTestCase : public TestCase {
private:
	ConnectionManager * cm;
	PollingThread * thread;
	int port;
public:
	ConnectionManagerMultiConnTestCase() : TestCase("ConnectionManagerMultiConnTestCase"), port(0) {}
	virtual ~ConnectionManagerMultiConnTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		port = env.getIntegerProperty("listen.port");
		cm = new ConnectionManager(
			ConnectionConfig(
				AutoRef<CommunicationMaker>(new EchoCommunicationMaker),
				5));
		cm->start(port);

		thread = new PollingThread(cm);
		thread->start();
	}
	virtual void tearDown() {

		thread->interrupt();
		thread->join();
		delete thread;

		cm->stop();
		delete cm;
	}
	virtual void test() {

		class EchoVisitorTask : public Task {
		private:
			int id;
			Expect & done;
			InetAddress remoteAddr;
		public:
			EchoVisitorTask(int id, Expect & done, InetAddress remoteAddr) : id(id), done(done), remoteAddr(remoteAddr) {}
			virtual ~EchoVisitorTask() {}
			virtual void onTask() {
				Socket sock(remoteAddr);
				sock.connect();
				try {
					string say = "yeah!";
					sock.send(say.c_str(), say.size());
					char buffer[1024] = {0,};
					if (sock.recv(buffer, sizeof(buffer)) > 0) {
						ASSERT(say, ==, buffer);
					}
					cout << " ** [" << id << "]EchoVisitorTask/complete" << endl;
				} catch (Exception & e) {
					cout << " ** [" << id << "]EchoVisitorTask/error: " << e.toString() << endl;
				}
				
				sock.close();
				done--;
			}
		};
		
		TaskThreadPool pool(10);
		pool.start();

		size_t cnt = 100;
		Expect done(cnt);
		InetAddress addr = cm->getServerAddress();
		for (size_t i = 0; i < cnt; i++) {
			pool.setTaskWaitIfFull(AutoRef<Task>(new EchoVisitorTask((int)i, done, InetAddress("127.0.0.1", addr.getPort()))));
		}

		done.wait();

		pool.stop();
	}
};

int main(int argc, char *args[]) {
	TestEnvironment env;
	env["listen.port"] = "0";
	TestSuite ts(env);
	ts.addTestCase(AutoRef<TestCase>(new BasicConnectionManagerTestCase));
	ts.addTestCase(AutoRef<TestCase>(new ConnectionManagerTestCase));
	ts.addTestCase(AutoRef<TestCase>(new ConnectionManagerMultiConnTestCase));
	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
	return 0;
}
