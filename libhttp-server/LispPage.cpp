#include <vector>
#include "LispPage.hpp"
#include "UrlEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/Iterator.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/FileStream.hpp>
#include <liboslayer/DatabaseDriver.hpp>
#include "BasicAuth.hpp"

#define _VAR OS::GCRef<LISP::Var> 
#define DECL_PROC() OS::GCRef<LISP::Var> proc(LISP::Env & env, AutoRef<LISP::Scope> scope, OS::GCRef<LISP::Var> name, vector<OS::GCRef<LISP::Var> > & args)
#define HEAP_ALLOC(E,V) E.alloc(new LISP::Var(V))

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);

	static string escapeText(const string & txt) {
		return Text::replaceAll(Text::replaceAll(txt, "\\", "\\\\"), "\"", "\\\"");
	}
	
	LispPage::LispPage() {
		LISP::native(_env);
	}
	
	LispPage::~LispPage() {
	}
	
	LISP::Env & LispPage::env() {
		return _env;
	}

	string LispPage::toLispySymbolName(const string & name) {
		string n = Text::replaceAll(Text::replaceAll(name, ".", "-"), "_", "-");
		return "*" + n + "*";
	}

	void LispPage::applyProperties(const map<string, string> & props) {
		for (map<string, string>::const_iterator iter = props.begin(); iter != props.end(); iter++) {
			env().scope()->put_sym(toLispySymbolName(iter->first), env().alloc(new LISP::Var(LISP::wrap_text(iter->second))));
		}
	}
	
	void LispPage::applyWeb(HttpServerConfig & config) {
		applyWeb(_env, config);
	}
	void LispPage::applyWeb(LISP::Env & env, HttpServerConfig & config) {
		class Enc : public LISP::Procedure {
		private:
		public:
			Enc(const string & name) : LISP::Procedure(name) {}
			virtual ~Enc() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				string txt = LISP::eval(env, scope, iter.next())->toString();
				return HEAP_ALLOC(env, LISP::wrap_text(UrlEncoder::encode(txt)));
			}
		};
		env.scope()->put_func("url-encode", HEAP_ALLOC(env, AutoRef<LISP::Procedure>(new Enc("url-encode"))));
        
		class Dec : public LISP::Procedure {
		private:
		public:
			Dec(const string & name) : LISP::Procedure(name) {}
			virtual ~Dec() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				return HEAP_ALLOC(env, LISP::wrap_text(UrlDecoder::decode(LISP::eval(env, scope, iter.next())->toString())));
			}
		};
		env.scope()->put_func("url-decode", HEAP_ALLOC(env, AutoRef<LISP::Procedure>(new Dec("url-decode"))));

		class Config : public LISP::Procedure {
		private:
			HttpServerConfig config;
		public:
			Config(const string & name, HttpServerConfig & config)
				: LISP::Procedure(name), config(config) {}
			virtual ~Config() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				string key = LISP::eval(env, scope, iter.next())->toString();
				return HEAP_ALLOC(env, LISP::wrap_text(config[key]));
			}
		};
		env.scope()->put_func("http:get-config", HEAP_ALLOC(env, AutoRef<LISP::Procedure>(new Config("http:get-config", config))));

	}
	void LispPage::applyAuth(HttpRequest & request, HttpResponse & response) {
		applyAuth(_env, request, response);
	}
	void LispPage::applyAuth(LISP::Env & env, HttpRequest & request, HttpResponse & response) {
		class Auth : public LISP::Procedure {
		private:
			HttpRequest & request;
			HttpResponse & response;
		public:
			Auth(const string & name, HttpRequest & request, HttpResponse & response)
				: LISP::Procedure(name), request(request), response(response) {}
			virtual ~Auth() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				if (name->r_symbol() == "proc-basic-auth") {
					string realm = LISP::eval(env, scope, iter.next())->toString();
					string username = LISP::eval(env, scope, iter.next())->toString();
					string password = LISP::eval(env, scope, iter.next())->toString();
					BasicAuth auth(realm, username, password);
					if (!auth.validate(request)) {
						auth.setAuthentication(response);
						return HEAP_ALLOC(env, "nil");
					}
					return HEAP_ALLOC(env, "t");
				}
				return HEAP_ALLOC(env, "nil");
			}
		};
		env.scope()->put_func("proc-basic-auth", HEAP_ALLOC(env, AutoRef<LISP::Procedure>(new Auth("auth", request, response))));
	}
	void LispPage::applySession(AutoRef<HttpSession> session) {
		applySession(_env, session);
	}
	void LispPage::applySession(LISP::Env & env, AutoRef<HttpSession> session) {
		class LispSession : public LISP::Procedure {
		private:
			AutoRef<HttpSession> session;
		public:
			LispSession(const string & name, AutoRef<HttpSession> session) :
				LISP::Procedure(name), session(session) {}
			virtual ~LispSession() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				if (name->r_symbol() == "url") {
					string url = LISP::eval(env, scope, iter.next())->toString();
					return HEAP_ALLOC(env, LISP::wrap_text(HttpSessionTool::urlMan(url, session)));
				} else if (name->r_symbol() == "get-session-value") {
					string name = LISP::eval(env, scope, iter.next())->toString();
					return HEAP_ALLOC(env, LISP::wrap_text((*session)[name]));
				} else if (name->r_symbol() == "set-session-value") {
					string name = LISP::eval(env, scope, iter.next())->toString();
					string value = LISP::eval(env, scope, iter.next())->toString();
					(*session)[name] = value;
					return HEAP_ALLOC(env, LISP::wrap_text(value));
				}
				return HEAP_ALLOC(env, "nil");
                
			}
		};
		AutoRef<LISP::Procedure> proc(new LispSession("url", session));
		env.scope()->put_func("url", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-session-value", HEAP_ALLOC(env, proc));
		env.scope()->put_func("set-session-value", HEAP_ALLOC(env, proc));
	}
	void LispPage::applyRequest(HttpRequest & request) {
		applyRequest(_env, request);
	}
	void LispPage::applyRequest(LISP::Env & env, HttpRequest & request) {
		class LispRequest : public LISP::Procedure {
		private:
			HttpRequest & request;
		public:
			LispRequest(const string & name, HttpRequest & request) :
				LISP::Procedure(name), request(request) {}
			virtual ~LispRequest() {}
			virtual DECL_PROC() {
				Iterator<_VAR> iter(args);
				if (name->r_symbol() == "get-request-method") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getMethod()));
				} else if (name->r_symbol() == "get-request-path") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getPath()));
				} else if (name->r_symbol() == "get-request-param") {
					string paramName = LISP::eval(env, scope, iter.next())->toString();
					return HEAP_ALLOC(env, LISP::wrap_text(request.getParameter(paramName)));
				} else if (name->r_symbol() == "get-request-header") {
					string paramName = LISP::eval(env, scope, iter.next())->toString();
					return HEAP_ALLOC(env, LISP::wrap_text(request.getHeaderFieldIgnoreCase(paramName)));
				} else if (name->r_symbol() == "get-remote-host") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getRemoteAddress().getHost()));
				} else if (name->r_symbol() == "get-remote-port") {
					return HEAP_ALLOC(env, LISP::Integer(request.getRemoteAddress().getPort()));
				}
				return HEAP_ALLOC(env, "nil");
			}
		};
		AutoRef<LISP::Procedure> proc(new LispRequest("request*", request));
		env.scope()->put_func("get-request-method", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-request-path", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-request-param", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-request-header", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-remote-host", HEAP_ALLOC(env, proc));
		env.scope()->put_func("get-remote-port", HEAP_ALLOC(env, proc));
	}

	void LispPage::applyResponse(HttpResponse & response) {
		applyResponse(_env, response);
	}
	void LispPage::applyResponse(LISP::Env & env, HttpResponse & response) {
		class LispResponse : public LISP::Procedure {
		private:
			HttpResponse & response;
		public:
			LispResponse(const string & name, HttpResponse & response) :
				LISP::Procedure(name), response(response) {}
			virtual ~LispResponse() {}
			virtual DECL_PROC() {
				Iterator<_VAR> iter(args);
				if (name->r_symbol() == "set-status-code") {
					int status = (int)LISP::eval(env, scope, iter.next())->r_integer().getInteger();
					string statusMessage;
					if (iter.has()) {
						statusMessage = LISP::eval(env, scope, iter.next())->toString();
					}
					if (statusMessage.empty()) {
						response.setStatus(status);
					} else {
						response.setStatus(status, statusMessage);
					}
					return HEAP_ALLOC(env, LISP::Integer(status));
				} else if (name->r_symbol() == "set-response-header") {
					string name = LISP::eval(env, scope, iter.next())->toString();
					string value = LISP::eval(env, scope, iter.next())->toString();
					response.setHeaderField(name, value);
					return HEAP_ALLOC(env, LISP::wrap_text(value));
				} else if (name->r_symbol() == "set-redirect") {
					string location = LISP::eval(env, scope, iter.next())->toString();
					response.setRedirect(location);
					return HEAP_ALLOC(env, LISP::wrap_text(location));
				} else if (name->r_symbol() == "set-forward") {
					string location = LISP::eval(env, scope, iter.next())->toString();
					response.setForward(location);
					return HEAP_ALLOC(env, LISP::wrap_text(location));
				} else if (name->r_symbol() == "set-file-transfer") {
					string path = LISP::eval(env, scope, iter.next())->toString();
					response["set-file-transfer"] = path;
					return HEAP_ALLOC(env, LISP::wrap_text(path));
				}
				return HEAP_ALLOC(env, "nil");
			}
		};
		AutoRef<LISP::Procedure> proc(new LispResponse("response*", response));
		env.scope()->put_func("set-status-code", HEAP_ALLOC(env, proc));
		env.scope()->put_func("set-response-header", HEAP_ALLOC(env, proc));
		env.scope()->put_func("set-redirect", HEAP_ALLOC(env, proc));
		env.scope()->put_func("set-forward", HEAP_ALLOC(env, proc));
		env.scope()->put_func("set-file-transfer", HEAP_ALLOC(env, proc));
		
	}
	void LispPage::applyLoadPage() {
		applyLoadPage(_env);
	}
	void LispPage::applyLoadPage(LISP::Env & env) {
		class LispLoadPage : public LISP::Procedure {
		private:
		public:
			LispLoadPage(const string & name) : LISP::Procedure(name) {}
			virtual ~LispLoadPage() {}
			virtual DECL_PROC() {
				Iterator<_VAR> iter(args);
				FileStream reader(LISP::pathname(env, LISP::eval(env, scope, iter.next()))->r_file(), "rb");
				return HEAP_ALLOC(env, LISP::wrap_text(LispPage::parseLispPage(env, reader.readFullAsString())));
			}
		};
		AutoRef<LISP::Procedure> proc(new LispLoadPage("load-page"));
		env.scope()->put_func("load-page", HEAP_ALLOC(env, proc));
	}
    
	bool LispPage::compile(LISP::Env & env, const string & line) {
		try {
			LISP::compile(env, line);
			env.gc();
		} catch (LISP::ExitLispException e) {
			throw e;
		} catch (OS::Exception & e) {
			logger->loge("ERROR: " + e.toString());
			return false;
		} catch (std::exception & e) {
			logger->loge("ERROR: " + string(e.what()));
			return false;
		}
		return true;
	}
	string LispPage::parseLispPage(const string & src) {
		return parseLispPage(_env, src);
	}
	string LispPage::convertLispPageToCode(const string & src) {
		vector<string> parts;
		size_t s = src.find("<%");
		size_t e = 0;
		while (s != string::npos) {
			string txt = src.substr(e, s - e);
			if (txt.empty() == false) {
				parts.push_back(txt);
			}
			e = src.find("%>", s + 2);
			if (e == string::npos) {
				throw Exception("format error - code block requires end tag '%>'");
			}
			string code = src.substr(s, e + 2 - s);
			parts.push_back(code);
			e += 2;
			s = src.find("<%", e);
		}
		if (e < src.size()) {
			parts.push_back(src.substr(e));
		}
		string ret;
		for (size_t i = 0; i < parts.size(); i++) {
			if (Text::startsWith(parts[i], "<%=")) {
				ret.append("(setq *content* (string-append *content* ");
				string p = parts[i].substr(3, parts[i].size() - 5);
				ret.append(p);
				ret.append("))");
			} else if (Text::startsWith(parts[i], "<%")) {
				string p = parts[i].substr(2, parts[i].size() - 4);
				ret.append(p);
			} else {
				ret.append("(setq *content* (string-append *content* \"");
				string p = escapeText(parts[i]);
				ret.append(p);
				ret.append("\"))");
			}
		}
		return ret;
	}

	string LispPage::parseLispPage(LISP::Env & env, const string & src) {

		compile(env, "(defparameter *content* \"\")");
		
		LISP::BufferedCommandReader reader;
		reader.read(convertLispPageToCode(src));
		vector<string> commands = reader.getCommands();
		try {
			for (vector<string>::iterator cmd = commands.begin(); cmd != commands.end(); cmd++) {
				compile(env, *cmd);
			}
		} catch (LISP::ExitLispException e) {
			logger->logd("[LispPage - (quit)]");
		}
		reader.clearCommands();

		return env.scope()->rget_sym("*content*")->toString();
	}
	
}
