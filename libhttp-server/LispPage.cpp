#include <vector>
#include "LispPage.hpp"
#include "HttpEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/Iterator.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/FileStream.hpp>
#include "BasicAuth.hpp"

#define _VAR OS::GCRef<LISP::Var> 
#define DECL_PROC() OS::GCRef<LISP::Var> proc(LISP::Env & env, LISP::Scope & scope, OS::GCRef<LISP::Var> name, vector<OS::GCRef<LISP::Var> > & args)
#define HEAP_ALLOC(E,V) E.alloc(new LISP::Var(V))

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getObservingLogger(__FILE__);
	
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
			env().scope().put(toLispySymbolName(iter->first), env().alloc(new LISP::Var(LISP::wrap_text(iter->second))));
		}
	}
	
	void LispPage::applyWeb() {
		applyWeb(_env);
	}
	void LispPage::applyWeb(LISP::Env & env) {
		class Enc : public LISP::Procedure {
		private:
		public:
			Enc(const string & name) : LISP::Procedure(name) {}
			virtual ~Enc() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				string txt = LISP::eval(env, scope, iter.next())->toString();
				return HEAP_ALLOC(env, LISP::wrap_text(HttpEncoder::encode(txt)));
			}
		};
		env.scope().put("url-encode", HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Enc("url-encode"))));
        
		class Dec : public LISP::Procedure {
		private:
		public:
			Dec(const string & name) : LISP::Procedure(name) {}
			virtual ~Dec() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				return HEAP_ALLOC(env, LISP::wrap_text(HttpDecoder::decode(LISP::eval(env, scope, iter.next())->toString())));
			}
		};
		env.scope().put("url-decode", HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Dec("url-decode"))));
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
		env.scope().put("proc-basic-auth", HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Auth("auth", request, response))));
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
		UTIL::AutoRef<LISP::Procedure> proc(new LispSession("url", session));
		env.scope().put("url", HEAP_ALLOC(env, proc));
		env.scope().put("get-session-value", HEAP_ALLOC(env, proc));
		env.scope().put("set-session-value", HEAP_ALLOC(env, proc));
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
				}
				return HEAP_ALLOC(env, "nil");
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispRequest("request*", request));
		env.scope().put("get-request-method", HEAP_ALLOC(env, proc));
		env.scope().put("get-request-path", HEAP_ALLOC(env, proc));
		env.scope().put("get-request-param", HEAP_ALLOC(env, proc));
		env.scope().put("get-request-header", HEAP_ALLOC(env, proc));
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
		UTIL::AutoRef<LISP::Procedure> proc(new LispResponse("response*", response));
		env.scope().put("set-status-code", HEAP_ALLOC(env, proc));
		env.scope().put("set-response-header", HEAP_ALLOC(env, proc));
		env.scope().put("set-redirect", HEAP_ALLOC(env, proc));
		env.scope().put("set-forward", HEAP_ALLOC(env, proc));
		env.scope().put("set-file-transfer", HEAP_ALLOC(env, proc));
		
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
		UTIL::AutoRef<LISP::Procedure> proc(new LispLoadPage("load-page"));
		env.scope().put("load-page", HEAP_ALLOC(env, proc));
	}
    
	bool LispPage::compile(LISP::Env & env, const string & line) {
		try {
			LISP::compile(env, line);
		} catch (LISP::ExitLispException & e) {
			logger->logd("Normal Exit");
			// exit by code
		} catch (OS::Exception & e) {
			logger->loge("ERROR: " + e.getMessage());
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
	string LispPage::parseLispPage(LISP::Env & env, const string & src) {
        
		size_t f = 0;
		size_t s = 0;

		if (env.scope().rsearch("*content*").nil()) {
			env.scope().put("*content*", HEAP_ALLOC(env, LISP::wrap_text("")));
		}
		while ((f = src.find("<%", f)) != string::npos) {
			if (f - s > 0) {
				string txt = src.substr(s, f - s);
				_VAR content = env.scope().rget("*content*");
				env.scope().rget("*content*") = HEAP_ALLOC(env, LISP::wrap_text(content->toString() + txt));
			}
			size_t e = src.find("%>", f);
			string code = src.substr(f + 2, e - (f + 2));
			if (*code.begin() == '=') {
				// print
				string line = Text::trim(code.substr(1));
				line = "(setq *content* (string-append *content* " + line + "))";
				compile(env, line);
			} else {
				// code
				vector<string> lines = Text::split(code, "\n");
				LISP::BufferedCommandReader reader;
				for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
					string line = *iter;
					if (!line.empty() && reader.read(line + " ") > 0) {
						vector<string> commands = reader.getCommands();
						for (vector<string>::iterator cmd = commands.begin(); cmd != commands.end(); cmd++) {
                            compile(env, *cmd);
						}
						reader.clearCommands();
					}
				}
			}
            
			s = f = e + 2;
		}
		if (s < src.length()) {
			string txt = src.substr(s);
			_VAR content = env.scope().rget("*content*");
			env.scope().rget("*content*") = HEAP_ALLOC(env, LISP::wrap_text(content->toString() + txt));
		}
        
		return env.scope().rget("*content*")->toString();
	}
}
