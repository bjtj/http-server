#include <vector>
#include "LispPage.hpp"
#include "HttpEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/FileReaderWriter.hpp>

namespace HTTP {

	using namespace std;
	using namespace UTIL;
	
	LispPage::LispPage() {
		LISP::native(global_env);
	}
	
	LispPage::~LispPage() {
	}
	
	LISP::Env & LispPage::env() {
		return global_env;
	}

	string LispPage::toLispySymbolName(const string & name) {
		string n = Text::replaceAll(Text::replaceAll(name, ".", "-"), "_", "-");
		return "*" + n + "*";
	}

	void LispPage::applyProperties(const map<string, string> & props) {
		for (map<string, string>::const_iterator iter = props.begin(); iter != props.end(); iter++) {
			env()[toLispySymbolName(iter->first)] = LISP::text(iter->second);
		}
	}
	
	void LispPage::applyWeb() {
		applyWeb(global_env);
	}
	void LispPage::applyWeb(LISP::Env & env) {
		class Enc : public LISP::Procedure {
		private:
		public:
			Enc(const string & name) : LISP::Procedure(name) {}
			virtual ~Enc() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {
				return LISP::text(HttpEncoder::encode(LISP::eval(args[0], env).toString()));
			}
		};
        
		class Dec : public LISP::Procedure {
		private:
		public:
			Dec(const string & name) : LISP::Procedure(name) {}
			virtual ~Dec() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {
				return LISP::text(HttpDecoder::decode(LISP::eval(args[0], env).toString()));
			}
		};
        
		env["url-encode"] = LISP::Var(UTIL::AutoRef<LISP::Procedure>(new Enc("url-encode")));
		env["url-decode"] = LISP::Var(UTIL::AutoRef<LISP::Procedure>(new Dec("url-decode")));
	}
	void LispPage::applySession(HttpSession & session) {
		applySession(global_env, session);
	}
	void LispPage::applySession(LISP::Env & env, HttpSession & session) {
		
		class LispSession : public LISP::Procedure {
		private:
			HttpSession & session;
		public:
			LispSession(const string & name, HttpSession & session) :
				LISP::Procedure(name), session(session) {}
			virtual ~LispSession() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {

				if (name.getSymbol() == "url") {
					string url = args[0].toString();
					return LISP::text(HttpSessionTool::urlMan(url, session));
				} else if (name.getSymbol() == "get-session-value") {
					string name = LISP::eval(args[0], env).toString();
					return LISP::text(session[name]);
				} else if (name.getSymbol() == "set-session-value") {
					string name = LISP::eval(args[0], env).toString();
					string value = LISP::eval(args[1], env).toString();
					session[name] = value;
					return LISP::text(value);
				}

				return "nil";
                
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispSession("url", session));
		env["url"] = LISP::Var(proc);
		env["get-session-value"] = LISP::Var(proc);
		env["set-session-value"] = LISP::Var(proc);
	}
	void LispPage::applyRequest(HttpRequest & request) {
		applyRequest(global_env, request);
	}
	void LispPage::applyRequest(LISP::Env & env, HttpRequest & request) {
		class LispRequest : public LISP::Procedure {
		private:
			HttpRequest & request;
		public:
			LispRequest(const string & name, HttpRequest & request) :
				LISP::Procedure(name), request(request) {}
			virtual ~LispRequest() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {
				string paramName = LISP::eval(args[0], env).toString();
				if (name.getSymbol() == "get-request-param") {
					return LISP::text(request.getParameter(paramName));
				} else if (name.getSymbol() == "get-request-header-field") {
					return LISP::text(request.getHeaderFieldIgnoreCase(paramName));
				}
				
				return "nil";
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispRequest("request*", request));
		env["get-request-param"] = LISP::Var(proc);
		env["get-request-header-field"] = LISP::Var(proc);
	}

	void LispPage::applyResponse(HttpResponse & response) {
		applyResponse(global_env, response);
	}
	void LispPage::applyResponse(LISP::Env & env, HttpResponse & response) {
		class LispResponse : public LISP::Procedure {
		private:
			HttpResponse & response;
		public:
			LispResponse(const string & name, HttpResponse & response) :
				LISP::Procedure(name), response(response) {}
			virtual ~LispResponse() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {

				if (name.getSymbol() == "set-status-code") {
					int status = (int)LISP::eval(args[0], env).getInteger().getInteger();
					response.setStatusCode(status);
					return LISP::Integer(status);
				} else if (name.getSymbol() == "set-response-header-field") {
					string name = LISP::eval(args[0], env).toString();
					string value = LISP::eval(args[1], env).toString();
					response.getHeader().setHeaderField(name, value);
					return LISP::text(value);
				} else if (name.getSymbol() == "set-redirect") {
					string location = LISP::eval(args[0], env).toString();
					response.setRedirect(location);
					return LISP::text(location);
				} else if (name.getSymbol() == "set-file-transfer") {
					string path = LISP::eval(args[0], env).toString();
					response["set-file-transfer"] = path;
					return LISP::text(path);
				}
				
				return "nil";
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispResponse("response*", response));
		env["set-status-code"] = LISP::Var(proc);
		env["set-response-header-field"] = LISP::Var(proc);
		env["set-redirect"] = LISP::Var(proc);
		env["set-file-transfer"] = LISP::Var(proc);
		
	}
	void LispPage::applyLoadPage() {
		applyLoadPage(global_env);
	}
	void LispPage::applyLoadPage(LISP::Env & env) {
		class LispLoadPage : public LISP::Procedure {
		private:
		public:
			LispLoadPage(const string & name) : LISP::Procedure(name) {}
			virtual ~LispLoadPage() {}
			virtual LISP::Var proc(LISP::Var name, vector<LISP::Var> & args, LISP::Env & env) {
				FileReader reader(LISP::pathname(LISP::eval(args[0], env)).getFile());
				return LISP::text(LispPage::parseLispPage(env, reader.dumpAsString()));
			}
		};
		UTIL::AutoRef<LISP::Procedure> proc(new LispLoadPage("load-page"));
		env["load-page"] = LISP::Var(proc);
	}
    
	bool LispPage::compile(const string & line, LISP::Env & env) {
		try {
			LISP::compile(line, env);
			return !env.quit();
		} catch (const char * e) {
			cout << "ERROR: " << e << endl;
			return false;
		} catch (const string & e) {
			cout << "ERROR: " << e << endl;
			return false;
		}
	}
	string LispPage::parseLispPage(const string & src) {
		return parseLispPage(global_env, src);
	}
	string LispPage::parseLispPage(LISP::Env & env, const string & src) {
        
		size_t f = 0;
		size_t s = 0;
		while (!env.quit() && (f = src.find("<%", f)) != string::npos) {
            
			if (f - s > 0) {
				string txt = src.substr(s, f - s);
				LISP::Var & content = env["*content*"];
				env["*content*"] = LISP::text(content.isNil() ? txt : content.toString() + txt);
			}
            
			size_t e = src.find("%>", f);
			string code = src.substr(f + 2, e - (f + 2));
            
			if (*code.begin() == '=') {
				string line = Text::trim(code.substr(1));
				line = "(setq *content* (string-append *content* " + line + "))";
				compile(line, env);
			} else {
				vector<string> lines = Text::split(code, "\n");
				LISP::BufferedCommandReader reader;
				for (vector<string>::iterator iter = lines.begin(); !env.quit() && iter != lines.end(); iter++) {
					string line = *iter;
					if (!line.empty() && reader.read(line + " ") > 0) {
						vector<string> commands = reader.getCommands();
						for (vector<string>::iterator cmd = commands.begin(); !env.quit() && cmd != commands.end(); cmd++) {
							if (!compile(*cmd, env)) {
								break;
							}
						}
						reader.clearCommands();
					}
				}
			}
            
			s = f = e + 2;
		}
		if (!env.quit() && s < src.length()) {
			string txt = src.substr(s);
			LISP::Var & content = env["*content*"];
			env["*content*"] = LISP::text(content.isNil() ? txt : content.toString() + txt);
		}
        
		return env["*content*"].toString();
	}
}
