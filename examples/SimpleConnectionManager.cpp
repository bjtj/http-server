#include <iostream>
#include <deque>
#include <vector>
#include <algorithm>
#include <liboslayer/os.hpp>
#include <liboslayer/Thread.hpp>
#include <liboslayer/Ref.hpp>
#include <liboslayer/AutoRef.hpp>
#include <libhttp-server/ConnectionManager.hpp>

using namespace std;
using namespace osl;
using namespace http;



/**
 *
 */
class Client
{
private:
    AutoRef<Connection> _conn;
public:
    Client(AutoRef<Connection> conn) : _conn(conn) {
    }
    virtual ~Client() {
    }
    bool operator== (const Client & client) {
        return _conn == client._conn;
    }
};

/**
 *
 */
class ClientList
{
private:
    vector<Client> _clients;
public:
    ClientList() {
    }
    virtual ~ClientList() {
    }
    void add(const Client & client) {
        _clients.push_back(client);
    }
    void remove(const Client & client) {
        _clients.erase(std::find(_clients.begin(), _clients.end(), client));
    }
};


class Application
{
private:
    ClientList _clients;
public:
    Application() {
    }
    virtual ~Application() {
    }
    ClientList & clients() {
        return _clients;
    }
};


/**
 *
 */
class MsgComm : public Communication, public CommunicationMaker
{
private:
    bool _done;
    Ref<Application> _app;
public:
    MsgComm(const Ref<Application> & app) : _done(false), _app(app) {
    }
    virtual ~MsgComm() {
    }
    virtual AutoRef<Communication> makeCommunication() {
        return AutoRef<Communication>(new MsgComm(_app));
    }
    virtual void onConnected(AutoRef<Connection> connection) {
        cout << "connected: " << connection->getRemoteAddress().getHost() << ":"
             << connection->getRemoteAddress().getPort() << endl;
    }
    virtual bool onReceivable(AutoRef<Connection> connection) {
        char buffer[1024] = {0,};
        connection->recv(buffer, sizeof(buffer));
        cout << buffer << endl;
        _done = true;
		return true;
    }
    virtual bool onWriteable(AutoRef<Connection> connection) {
		return true;
    }
    virtual void onDisconnected(AutoRef<Connection> connection) {
        cout << "disconnected: " << connection->getRemoteAddress().getHost() << ":"
             << connection->getRemoteAddress().getPort() << endl;
    }
    virtual bool isCommunicationCompleted() {
        return _done;
    }
};


/**
 *
 */
class ServerThread : public Thread
{
private:
    bool _finish;
    int _port;
    ConnectionManager _cm;
public:
    ServerThread(Ref<Application> app, int port)
        : _finish(false),
          _port(port),
          _cm(ConnectionConfig(AutoRef<CommunicationMaker>(new MsgComm(app)), 5)) {
    }
    virtual ~ServerThread() {
    }
    virtual void run() {
        _cm.start(_port);
		cout << "connection manager port: " << _cm.getServerAddress().getPort() << endl;
        while (!_finish && !interrupted()) {
            _cm.poll(100);
        }
        _cm.stop();
    }
};

/**
 *
 */
int main(int argc, char *argv[])
{
    Application app;
    ServerThread st(Ref<Application>(&app), 0);
    st.start();

    while (1) {
        string cmd;
        cin >> cmd;
        if (cmd == "quit" || cmd == "q") {
            break;
        }
    }

    st.interrupt();
    st.join();

    cout << "QUIT" << endl;

    return 0;
}
