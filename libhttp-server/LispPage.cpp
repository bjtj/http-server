#include <vector>
#include "LispPage.hpp"
#include "HttpEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/Iterator.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/FileStream.hpp>
#include "BasicAuth.hpp"

#define _VAR OS::Obj<LISP::Var> 
#define DECL_PROC() OS::Obj<LISP::Var> proc(OS::Obj<LISP::Var> name, vector<OS::Obj<LISP::Var> > & args, LISP::Env & env)
#define HEAP_ALLOC(E,V) E.alloc(new LISP::Var(V))

namespace HTTP {

	using namespace std;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::getInstance().getLogger(__FILE__);
	
	LispPage::LispPage() {
		LISP::native(_env);
	}

	LispPage::LispPage(LISP::Env * parentEnv) : _env(parentEnv) {
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
			env()[toLispySymbolName(iter->first)] = env().alloc(new LISP::Var(LISP::text(iter->second)));
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
				return HEAP_ALLOC(env, LISP::text(HttpEncoder::encode(LISP::eval(iter.next(), env)->toString())));
			}
		};
        
		class Dec : public LISP::Procedure {
		private:
		public:
			Dec(const string & name) : LISP::Procedure(name) {}
			virtual ~Dec() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				return HEAP_ALLOC(env, LISP::text(HttpDecoder::decode(LISP::eval(iter.next(), env)->toString())));
			}
		};
        
		env.local()["url-encode"] = HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Enc("url-encode")));
		env.local()["url-decode"] = HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Dec("url-decode")));
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
			Auth(const string & name, HttpRequest & request, HttpResponse & response) : LISP::Procedure(name), request(request), response(response) {}
			virtual ~Auth() {}
			virtual DECL_PROC() {
				Iterator<_VAR > iter(args);
				if (name->getSymbol() == "proc-basic-auth") {
					string realm = LISP::eval(iter.next(), env)->toString();
					string username = LISP::eval(iter.next(), env)->toString();
					string password = LISP::eval(iter.next(), env)->toString();

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
		env.local()["proc-basic-auth"] = HEAP_ALLOC(env, UTIL::AutoRef<LISP::Procedure>(new Auth("auth", request, response)));
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
				if (name->getSymbol() == "url") {
					string url = iter.next()->toString();
					return HEAP_ALLOC(env, LISP::text(HttpSessionTool::urlMan(url, session)));
				} else if (name->getSymbol() == "get-session-value") {
					string name = LISP::eval(iter.next(), env)->toString();
					return HEAP_ALLOC(env, LISP::text((*session)[name]));
				} else if (name->getSymbol() == "set-session-value") {
					string name = LISP::eval(iter.next(), env)->toString();
					string value = LISP::eval(iter.next(), env)->toString();
					(*session)[name] = value;
					return HEAP_ALLOC(env, LISP::text(value));
				}

				return HEAP_ALLOC(env, "nil");
                
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispSession("url", session));
		env.local()["url"] = HEAP_ALLOC(env, proc);
		env.local()["get-session-value"] = HEAP_ALLOC(env, proc);
		env.local()["set-session-value"] = HEAP_ALLOC(env, proc);
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
				if (name->getSymbol() == "get-request-method") {
					return HEAP_ALLOC(env, LISP::text(request.getMethod()));
				} else if (name->getSymbol() == "get-request-path") {
					return HEAP_ALLOC(env, LISP::text(request.getPath()));
				} else if (name->getSymbol() == "get-request-param") {
					string paramName = LISP::eval(iter.next(), env)->toString();
					return HEAP_ALLOC(env, LISP::text(request.getParameter(paramName)));
				} else if (name->getSymbol() == "get-request-header") {
					string paramName = LISP::eval(iter.next(), env)->toString();
					return HEAP_ALLOC(env, LISP::text(request.getHeaderFieldIgnoreCase(paramName)));
				}
				
				return HEAP_ALLOC(env, "nil");
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispRequest("request*", request));
		env.local()["get-request-method"] = HEAP_ALLOC(env, proc);
		env.local()["get-request-path"] = HEAP_ALLOC(env, proc);
		env.local()["get-request-param"] = HEAP_ALLOC(env, proc);
		env.local()["get-request-header"] = HEAP_ALLOC(env, proc);
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
				if (name->getSymbol() == "set-status-code") {
					int status = (int)LISP::eval(iter.next(), env)->getInteger().getInteger();
					response.setStatus(status);
					return HEAP_ALLOC(env, LISP::Integer(status));
				} else if (name->getSymbol() == "set-response-header") {
					string name = LISP::eval(iter.next(), env)->toString();
					string value = LISP::eval(iter.next(), env)->toString();
					response.getHeader().setHeaderField(name, value);
					return HEAP_ALLOC(env, LISP::text(value));
				} else if (name->getSymbol() == "set-redirect") {
					string location = LISP::eval(iter.next(), env)->toString();
					response.setRedirect(location);
					return HEAP_ALLOC(env, LISP::text(location));
				} else if (name->getSymbol() == "set-file-transfer") {
					string path = LISP::eval(iter.next(), env)->toString();
					response["set-file-transfer"] = path;
					return HEAP_ALLOC(env, LISP::text(path));
				}
				
				return HEAP_ALLOC(env, "nil");
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispResponse("response*", response));
		env.local()["set-status-code"] = HEAP_ALLOC(env, proc);
		env.local()["set-response-header"] = HEAP_ALLOC(env, proc);
		env.local()["set-redirect"] = HEAP_ALLOC(env, proc);
		env.local()["set-file-transfer"] = HEAP_ALLOC(env, proc);
		
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
				FileStream reader(LISP::pathname(env, LISP::eval(iter.next(), env))->getFile(), "rb");
				return HEAP_ALLOC(env, LISP::text(LispPage::parseLispPage(env, reader.readFullAsString())));
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispLoadPage("load-page"));
		env.local()["load-page"] = HEAP_ALLOC(env, proc);
	}
    
	bool LispPage::compile(const string & line, LISP::Env & env) {
		try {
			LISP::compile(line, env);
			return !env.quit();
		} catch (OS::Exception & e) {
			logger->loge("ERROR: " + e.getMessage());
			return false;
		}
	}
	string LispPage::parseLispPage(const string & src) {
		return parseLispPage(_env, src);
	}
	string LispPage::parseLispPage(LISP::Env & env, const string & src) {
        
		size_t f = 0;
		size_t s = 0;

		if (env.local()["*content*"].nil()) {
			env.local()["*content*"] = HEAP_ALLOC(env, LISP::text(""));
		}

		while (!env.quit() && (f = src.find("<%", f)) != string::npos) {
            
			if (f - s > 0) {
				string txt = src.substr(s, f - s);
				_VAR content = env.local()["*content*"];
				env.local()["*content*"] = HEAP_ALLOC(env, LISP::text(content->toString() + txt));
			}
            
			size_t e = src.find("%>", f);
			string code = src.substr(f + 2, e - (f + 2));
            
			if (*code.begin() == '=') {
				// print
				string line = Text::trim(code.substr(1));
				line = "(setq *content* (string-append *content* " + line + "))";
				compile(line, env);
			} else {
				// code
				vector<string> lines = Text::split(code, "\n");
				LISP::BufferedCommandReader reader;
				for (vector<string>::iterator iter = lines.begin(); !env.quit() && iter != lines.end(); iter++) {
					string line = *iter;
					if (!line.empty() && reader.read(line + " ") > 0) {
						vector<string> commands = reader.getCommands();
						for (vector<string>::iterator cmd = commands.begin(); !env.quit() && cmd != commands.end(); cmd++) {
                            compile(*cmd, env);
						}
						reader.clearCommands();
					}
				}
			}
            
			s = f = e + 2;
		}
		if (!env.quit() && s < src.length()) {
			string txt = src.substr(s);
			_VAR content = env.local()["*content*"];
			env.local()["*content*"] = HEAP_ALLOC(env, LISP::text(content->toString() + txt));
		}
        
		return env.get("*content*")->toString();
	}
}
