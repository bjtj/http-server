#include <vector>
#include "LispPage.hpp"
#include "UrlEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/Iterator.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/FileStream.hpp>
#include <liboslayer/DatabaseDriver.hpp>
#include "BasicAuth.hpp"

#ifdef PLATFORM_APPLE
#   include <iconv.h>
#endif

#define _VAR OS::GCRef<LISP::Var> 
#define HEAP_ALLOC(E,V) E.alloc(new LISP::Var(V))

namespace HTTP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static AutoRef<Logger> logger = LoggerFactory::inst().getObservingLogger(__FILE__);

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
			env().scope()->put_var(LISP::Symbol(toLispySymbolName(iter->first)),
								   env().alloc(new LISP::Var(LISP::wrap_text(iter->second))));
		}
	}
	
	void LispPage::applyWeb(HttpServerConfig & config) {
		applyWeb(_env, config);
	}
	void LispPage::applyWeb(LISP::Env & env, HttpServerConfig & config) {
		class Enc : public LISP::Procedure {
		private:
		public:
			Enc() {}
			virtual ~Enc() {}
            LISP_PROCEDURE_PROC(env, scope, name, args) {
                Iterator<_VAR > iter = args.iter();
                string txt = LISP::eval(env, scope, iter.next())->toPrintString();
#ifdef PLATFORM_APPLE
                txt = resolve_utf8_mac(txt);
#endif
                return HEAP_ALLOC(env, LISP::wrap_text(UrlEncoder::encode(txt)));
            }

#ifdef PLATFORM_APPLE
            static string resolve_utf8_mac(const string & str) {
                string ret = str;
                char * utf8 = new char[str.size() + 1];
                try {
                    memset(utf8, 0, str.size() + 1);
                    resolve_utf8_mac(str.c_str(), utf8, str.size());
                    ret = string(utf8);
                } catch (const char * err) {
                    logger->error(err);
                }
                delete[] utf8;
                return ret;
            }
            
            // https://www.gnu.org/software/libc/manual/html_node/iconv-Examples.html
            static void resolve_utf8_mac(const char * str, char * out_buf, size_t avail) {
                size_t in_size = strlen(str);
                char * in_ptr = (char*)str;
                char * out_ptr = (char*)out_buf;
                iconv_t ic = iconv_open("UTF-8", "UTF-8-MAC");
                if (iconv(ic, &in_ptr, &in_size, &out_ptr, &avail) == (size_t)-1) {
                    iconv_close(ic);
                    throw Exception("iconv() failed");
                }
                iconv_close(ic);
            }
#endif
		};
		env.scope()->put_func(LISP::Symbol("url-encode"), HEAP_ALLOC(env, new Enc));
        
		class Dec : public LISP::Procedure {
		private:
		public:
			Dec() {}
			virtual ~Dec() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR > iter = args.iter();
				return HEAP_ALLOC(env, LISP::wrap_text(UrlDecoder::decode(LISP::eval(env, scope, iter.next())->toPrintString())));
			}
		};
		env.scope()->put_func(LISP::Symbol("url-decode"), HEAP_ALLOC(env, new Dec));

		class Config : public LISP::Procedure {
		private:
			HttpServerConfig config;
		public:
			Config(HttpServerConfig & config) : config(config) {}
			virtual ~Config() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR > iter = args.iter();
				string key = LISP::eval(env, scope, iter.next())->toPrintString();
				return HEAP_ALLOC(env, LISP::wrap_text(config[key]));
			}
		};
		env.scope()->put_func(LISP::Symbol("http:get-config"), HEAP_ALLOC(env, new Config(config)));

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
			Auth(HttpRequest & request, HttpResponse & response)
				: request(request), response(response) {}
			virtual ~Auth() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {

				class _cls : public OnBasicAuth {
				private:
					LISP::Env & _env;
					OS::UnsafeAutoRef<LISP::Scope> & _scope;
					_VAR _func;
				public:
					_cls(LISP::Env & env, OS::UnsafeAutoRef<LISP::Scope> & scope, _VAR func)
						: _env(env), _scope(scope), _func(func) {
					}
					virtual ~_cls() {
					}
					virtual bool onAuth(const string & username, const string & password) {
						_VAR name;
						_VAR un = HEAP_ALLOC(_env, LISP::wrap_text(username));
						_VAR pw = HEAP_ALLOC(_env, LISP::wrap_text(password));
						LISP::Sequence fargs;
						fargs.push_back(un);
						fargs.push_back(pw);
						return (_func->proc(_env, _scope, name, fargs)->isNil() == false);
					}
				};
				
				Iterator<_VAR > iter = args.iter();
				if (name->r_symbol() == "proc-basic-auth") {
					string realm = LISP::eval(env, scope, iter.next())->toPrintString();
					_VAR func = LISP::eval(env, scope, iter.next());
					BasicAuth auth(realm, AutoRef<OnBasicAuth>(new _cls(env, scope, func)));
					if (!auth.validate(request)) {
						auth.setAuthentication(response);
						return env.nil();
					}
					return env.t();
				}
				return env.nil();
			}
		};
		env.scope()->put_func(LISP::Symbol("proc-basic-auth"), HEAP_ALLOC(env, new Auth(request, response)));
	}
	void LispPage::applySession(HttpRequest & request, AutoRef<HttpSession> session) {
		applySession(_env, request, session);
	}
	void LispPage::applySession(LISP::Env & env, HttpRequest & request, AutoRef<HttpSession> session) {
		class LispSession : public LISP::Procedure {
		private:
			HttpRequest & request;
			AutoRef<HttpSession> session;
		public:
			LispSession(HttpRequest & request, AutoRef<HttpSession> session)
				: request(request), session(session) {}
			virtual ~LispSession() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR > iter = args.iter();
				if (name->r_symbol() == "url") {
					string url = LISP::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, LISP::wrap_text(HttpSessionTool::url(request, url, session)));
				} else if (name->r_symbol() == "get-session-id") {
					return HEAP_ALLOC(env, LISP::wrap_text(session->id()));
				} else if (name->r_symbol() == "get-session-value") {
					string name = LISP::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, LISP::wrap_text((*session)[name]));
				} else if (name->r_symbol() == "set-session-value") {
					string name = LISP::eval(env, scope, iter.next())->toPrintString();
					string value = LISP::eval(env, scope, iter.next())->toPrintString();
					(*session)[name] = value;
					return HEAP_ALLOC(env, LISP::wrap_text(value));
				}
				return env.nil();
                
			}
		};
		_VAR func = HEAP_ALLOC(env, new LispSession(request, session));
		env.scope()->put_func(LISP::Symbol("url"), func);
		env.scope()->put_func(LISP::Symbol("get-session-id"), func);
		env.scope()->put_func(LISP::Symbol("get-session-value"), func);
		env.scope()->put_func(LISP::Symbol("set-session-value"), func);
	}
	void LispPage::applyRequest(HttpRequest & request) {
		applyRequest(_env, request);
	}
	void LispPage::applyRequest(LISP::Env & env, HttpRequest & request) {
		class LispRequest : public LISP::Procedure {
		private:
			HttpRequest & request;
		public:
			LispRequest(HttpRequest & request)
				: request(request) {}
			virtual ~LispRequest() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				if (name->r_symbol() == "get-request-method") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getMethod()));
				} else if (name->r_symbol() == "get-request-path") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getPath()));
				} else if (name->r_symbol() == "get-request-raw-path") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getRawPath()));
				} else if (name->r_symbol() == "get-request-param") {
					string paramName = LISP::eval(env, scope, iter.next())->toPrintString();
					if (request.hasParameter(paramName) == false) {
						return env.nil();
					}
					return HEAP_ALLOC(env, LISP::wrap_text(request.getParameter(paramName)));
				} else if (name->r_symbol() == "get-request-header") {
					string paramName = LISP::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, LISP::wrap_text(request.getHeaderFieldIgnoreCase(paramName)));
				} else if (name->r_symbol() == "get-remote-host") {
					return HEAP_ALLOC(env, LISP::wrap_text(request.getRemoteAddress().getHost()));
				} else if (name->r_symbol() == "get-remote-port") {
					return HEAP_ALLOC(env, LISP::Integer(request.getRemoteAddress().getPort()));
				} else if (name->r_symbol() == "get-cookie") {
					string key = LISP::eval(env, scope, iter.next())->toPrintString();
					vector<Cookie> cookies = request.getCookies();
					for (Iterator<Cookie> iter(cookies); iter.avail(); iter++) {
						Cookie & cookie = *iter;
						if (cookie[key].empty() == false) {
							return HEAP_ALLOC(env, LISP::wrap_text(cookie[key]));
						}
					}
					return env.nil();
				} else if (name->r_symbol() == "get-all-cookies") {
					vector<Cookie> cookies = request.getCookies();
					vector<_VAR> lst;
					for (Iterator<Cookie> iter(cookies); iter.avail(); iter++) {
						Cookie & cookie = *iter;
						lst.push_back(HEAP_ALLOC(env, LISP::wrap_text(cookie.toString())));
					}
					return HEAP_ALLOC(env, lst);
				}
				return env.nil();
			}
		};
		_VAR func = HEAP_ALLOC(env, new LispRequest(request));
		env.scope()->put_func(LISP::Symbol("get-request-method"), func);
		env.scope()->put_func(LISP::Symbol("get-request-path"), func);
		env.scope()->put_func(LISP::Symbol("get-request-raw-path"), func);
		env.scope()->put_func(LISP::Symbol("get-request-param"), func);
		env.scope()->put_func(LISP::Symbol("get-request-header"), func);
		env.scope()->put_func(LISP::Symbol("get-remote-host"), func);
		env.scope()->put_func(LISP::Symbol("get-remote-port"), func);
		env.scope()->put_func(LISP::Symbol("get-cookie"), func);
		env.scope()->put_func(LISP::Symbol("get-all-cookies"), func);
	}

	void LispPage::applyResponse(HttpResponse & response) {
		applyResponse(_env, response);
	}
	void LispPage::applyResponse(LISP::Env & env, HttpResponse & response) {
		class LispResponse : public LISP::Procedure {
		private:
			HttpResponse & response;
		public:
			LispResponse(HttpResponse & response)
				: response(response) {}
			virtual ~LispResponse() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				if (name->r_symbol() == "set-status-code") {
					int status = (int)LISP::eval(env, scope, iter.next())->r_integer().getInteger();
					string statusMessage;
					if (iter.avail()) {
						statusMessage = LISP::eval(env, scope, iter.next())->toPrintString();
					}
					if (statusMessage.empty()) {
						response.setStatus(status);
					} else {
						response.setStatus(status, statusMessage);
					}
					return HEAP_ALLOC(env, LISP::Integer(status));
				} else if (name->r_symbol() == "set-response-header") {
					string name = LISP::eval(env, scope, iter.next())->toPrintString();
					string value = LISP::eval(env, scope, iter.next())->toPrintString();
					response.setHeaderField(name, value);
					return HEAP_ALLOC(env, LISP::wrap_text(value));
				} else if (name->r_symbol() == "set-redirect") {
					string location = LISP::eval(env, scope, iter.next())->toPrintString();
					response.setRedirect(location);
					return HEAP_ALLOC(env, LISP::wrap_text(location));
				} else if (name->r_symbol() == "set-forward") {
					string location = LISP::eval(env, scope, iter.next())->toPrintString();
					response.setForward(location);
					return HEAP_ALLOC(env, LISP::wrap_text(location));
				} else if (name->r_symbol() == "set-file-transfer") {
					string path = LISP::eval(env, scope, iter.next())->toPrintString();
					response["set-file-transfer"] = path;
					return HEAP_ALLOC(env, LISP::wrap_text(path));
				} else if (name->r_symbol() == "push-set-cookie") {
					string cookie = LISP::eval(env, scope, iter.next())->toPrintString();
					vector<Cookie> cookies;
					cookies.push_back(Cookie(cookie));
					response.appendCookies(cookies);
					return HEAP_ALLOC(env, LISP::wrap_text(cookie));
				}
				return env.nil();
			}
		};
		_VAR func = HEAP_ALLOC(env, new LispResponse(response));
		env.scope()->put_func(LISP::Symbol("set-status-code"), func);
		env.scope()->put_func(LISP::Symbol("set-response-header"), func);
		env.scope()->put_func(LISP::Symbol("set-redirect"), func);
		env.scope()->put_func(LISP::Symbol("set-forward"), func);
		env.scope()->put_func(LISP::Symbol("set-file-transfer"), func);
		env.scope()->put_func(LISP::Symbol("push-set-cookie"), func);
		
	}
	void LispPage::applyLoadPage() {
		applyLoadPage(_env);
	}
	void LispPage::applyLoadPage(LISP::Env & env) {
		class LispLoadPage : public LISP::Procedure {
		private:
		public:
			LispLoadPage() {}
			virtual ~LispLoadPage() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				FileStream reader(LISP::pathname(env, LISP::eval(env, scope, iter.next()))->r_pathname().file(), "rb");
				return HEAP_ALLOC(env, LISP::wrap_text(LispPage::parseLispPage(env, reader.readFullAsString())));
			}
		};
		env.scope()->put_func(LISP::Symbol("load-page"), HEAP_ALLOC(env, new LispLoadPage));
	}
    
	bool LispPage::compile(LISP::Env & env, const string & line) {
		try {
			LISP::compile(env, line);
			env.gc();
		} catch (LISP::ExitLispException e) {
			throw e;
		} catch (OS::Exception & e) {
			logger->error("ERROR: '" + e.toString() + "'");
			return false;
		} catch (std::exception & e) {
			logger->error("ERROR: '" + string(e.what()) + "'");
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
				if (compile(env, *cmd) == false) {
					string errorMessage = "Error occurred with '" + *cmd + "'";
					logger->error(errorMessage);
					throw Exception("Wrong opertation");
				}
			}
		} catch (LISP::ExitLispException e) {
			logger->debug("[LispPage - (quit)]");
		}

		return env.scope()->rget_var(LISP::Symbol("*content*"))->toPrintString();
	}
	
}
