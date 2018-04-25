#include <vector>
#include "LispPage.hpp"
#include "UrlEncoderDecoder.hpp"
#include "HttpSessionTool.hpp"
#include <liboslayer/Iterator.hpp>
#include <liboslayer/Logger.hpp>
#include <liboslayer/File.hpp>
#include <liboslayer/FileStream.hpp>
#include <liboslayer/DatabaseDriver.hpp>
#include "BasicAuth.hpp"

#ifdef PLATFORM_APPLE
#   include <iconv.h>
#endif

#define _VAR osl::GCRef<lisp::Var> 
#define HEAP_ALLOC(E,V) E.alloc(new lisp::Var(V))

namespace http {

	using namespace std;
	using namespace osl;
	using namespace lisp;


	static AutoRef<Logger> logger = LoggerFactory::instance().
		getObservingLogger(File::basename(__FILE__));

	static string escapeText(const string & txt) {
		return Text::replaceAll(Text::replaceAll(txt, "\\", "\\\\"), "\"", "\\\"");
	}
	
    LispPage::LispPage() {
		lisp::native(_env);
	}
	
	LispPage::~LispPage() {
	}
	
	lisp::Env & LispPage::env() {
		return _env;
	}

	string LispPage::toLispySymbolName(const string & name) {
		string n = Text::replaceAll(Text::replaceAll(name, ".", "-"), "_", "-");
		return "*" + n + "*";
	}

	void LispPage::applyProperties(const map<string, string> & props) {
		for (map<string, string>::const_iterator iter = props.begin(); iter != props.end(); iter++) {
			env().scope()->put_var(lisp::Symbol(toLispySymbolName(iter->first)),
								   env().alloc(new lisp::Var(lisp::wrap_text(iter->second))));
		}
	}
	
	void LispPage::applyWeb(HttpServerConfig & config) {
		class Enc : public lisp::Procedure {
		private:
		public:
			Enc() {}
			virtual ~Enc() {}
            LISP_PROCEDURE_PROC(env, scope, name, args) {
                Iterator<_VAR > iter = args.iter();
                string txt = lisp::eval(env, scope, iter.next())->toPrintString();
#ifdef PLATFORM_APPLE
                txt = resolve_utf8_mac(txt);
#endif
                return HEAP_ALLOC(env, lisp::wrap_text(UrlEncoder::encode(txt)));
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
		_env.scope()->put_func(lisp::Symbol("url-encode"), HEAP_ALLOC(_env, new Enc));
        
		class Dec : public lisp::Procedure {
		private:
		public:
			Dec() {}
			virtual ~Dec() {}
			LISP_PROCEDURE_PROC(_env, scope, name, args) {
				Iterator<_VAR > iter = args.iter();
				return HEAP_ALLOC(_env, lisp::wrap_text(UrlDecoder::decode(lisp::eval(_env, scope, iter.next())->toPrintString())));
			}
		};
		_env.scope()->put_func(lisp::Symbol("url-decode"), HEAP_ALLOC(_env, new Dec));

		class Config : public lisp::Procedure {
		private:
			HttpServerConfig config;
		public:
			Config(HttpServerConfig & config) : config(config) {}
			virtual ~Config() {}
			LISP_PROCEDURE_PROC(_env, scope, name, args) {
				Iterator<_VAR > iter = args.iter();
				string key = lisp::eval(_env, scope, iter.next())->toPrintString();
				return HEAP_ALLOC(_env, lisp::wrap_text(config[key]));
			}
		};
		_env.scope()->put_func(lisp::Symbol("http:get-config"), HEAP_ALLOC(_env, new Config(config)));

	}
	void LispPage::applyAuth(HttpRequest & request, HttpResponse & response) {
		class Auth : public lisp::Procedure {
		private:
			HttpRequest & request;
			HttpResponse & response;
		public:
			Auth(HttpRequest & request, HttpResponse & response)
				: request(request), response(response) {}
			virtual ~Auth() {}
			LISP_PROCEDURE_PROC(_env, scope, name, args) {

				class _cls : public OnBasicAuth {
				private:
					lisp::Env & _env;
					osl::UnsafeAutoRef<lisp::Scope> & _scope;
					_VAR _func;
				public:
					_cls(lisp::Env & env, osl::UnsafeAutoRef<lisp::Scope> & scope, _VAR func)
						: _env(env), _scope(scope), _func(func) {
					}
					virtual ~_cls() {
					}
					virtual bool onAuth(const string & username, const string & password) {
						_VAR name;
						_VAR un = HEAP_ALLOC(_env, lisp::wrap_text(username));
						_VAR pw = HEAP_ALLOC(_env, lisp::wrap_text(password));
						lisp::Sequence fargs;
						fargs.push_back(un);
						fargs.push_back(pw);
						return (_func->proc(_env, _scope, name, fargs)->isNil() == false);
					}
				};
				
				Iterator<_VAR > iter = args.iter();
				if (name->r_symbol() == "proc-basic-auth") {
					string realm = lisp::eval(_env, scope, iter.next())->toPrintString();
					_VAR func = lisp::eval(_env, scope, iter.next());
					BasicAuth auth(realm, AutoRef<OnBasicAuth>(new _cls(_env, scope, func)));
					if (!auth.validate(request)) {
						auth.setAuthentication(response);
						return _env.nil();
					}
					return _env.t();
				}
				return _env.nil();
			}
		};
		_env.scope()->put_func(lisp::Symbol("proc-basic-auth"), HEAP_ALLOC(_env, new Auth(request, response)));
	}
	void LispPage::applySession(HttpRequest & request, AutoRef<HttpSession> session) {
		class LispSession : public lisp::Procedure {
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
					string url = lisp::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, lisp::wrap_text(HttpSessionTool::url(request, url, session)));
				} else if (name->r_symbol() == "get-session-id") {
					return HEAP_ALLOC(env, lisp::wrap_text(session->id()));
				} else if (name->r_symbol() == "get-session-value") {
					string name = lisp::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, lisp::wrap_text((*session)[name]));
				} else if (name->r_symbol() == "set-session-value") {
					string name = lisp::eval(env, scope, iter.next())->toPrintString();
					string value = lisp::eval(env, scope, iter.next())->toPrintString();
					(*session)[name] = value;
					return HEAP_ALLOC(env, lisp::wrap_text(value));
				}
				return env.nil();
                
			}
		};
		_VAR func = HEAP_ALLOC(_env, new LispSession(request, session));
		_env.scope()->put_func(lisp::Symbol("url"), func);
		_env.scope()->put_func(lisp::Symbol("get-session-id"), func);
		_env.scope()->put_func(lisp::Symbol("get-session-value"), func);
		_env.scope()->put_func(lisp::Symbol("set-session-value"), func);
	}
	void LispPage::applyRequest(HttpRequest & request) {
		class LispRequest : public lisp::Procedure {
		private:
			HttpRequest & request;
		public:
			LispRequest(HttpRequest & request)
				: request(request) {}
			virtual ~LispRequest() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				if (name->r_symbol() == "get-request-method") {
					return HEAP_ALLOC(env, lisp::wrap_text(request.getMethod()));
				} else if (name->r_symbol() == "get-request-path") {
					return HEAP_ALLOC(env, lisp::wrap_text(request.getPath()));
				} else if (name->r_symbol() == "get-request-raw-path") {
					return HEAP_ALLOC(env, lisp::wrap_text(request.getRawPath()));
				} else if (name->r_symbol() == "get-request-param") {
					string paramName = lisp::eval(env, scope, iter.next())->toPrintString();
					if (request.hasParameter(paramName) == false) {
						return env.nil();
					}
					return HEAP_ALLOC(env, lisp::wrap_text(request.getParameter(paramName)));
				} else if (name->r_symbol() == "get-request-header") {
					string paramName = lisp::eval(env, scope, iter.next())->toPrintString();
					return HEAP_ALLOC(env, lisp::wrap_text(request.getHeaderField(paramName)));
				} else if (name->r_symbol() == "get-remote-host") {
					return HEAP_ALLOC(env, lisp::wrap_text(request.getRemoteAddress().getHost()));
				} else if (name->r_symbol() == "get-remote-port") {
					return HEAP_ALLOC(env, lisp::Integer(request.getRemoteAddress().getPort()));
				} else if (name->r_symbol() == "get-cookie") {
					string key = lisp::eval(env, scope, iter.next())->toPrintString();
					vector<Cookie> cookies = request.getCookies();
					for (Iterator<Cookie> iter(cookies); iter.avail(); iter++) {
						Cookie & cookie = *iter;
						if (cookie[key].empty() == false) {
							return HEAP_ALLOC(env, lisp::wrap_text(cookie[key]));
						}
					}
					return env.nil();
				} else if (name->r_symbol() == "get-all-cookies") {
					vector<Cookie> cookies = request.getCookies();
					vector<_VAR> lst;
					for (Iterator<Cookie> iter(cookies); iter.avail(); iter++) {
						Cookie & cookie = *iter;
						lst.push_back(HEAP_ALLOC(env, lisp::wrap_text(cookie.toString())));
					}
					return HEAP_ALLOC(env, lst);
				}
				return env.nil();
			}
		};
		_VAR func = HEAP_ALLOC(_env, new LispRequest(request));
		_env.scope()->put_func(lisp::Symbol("get-request-method"), func);
		_env.scope()->put_func(lisp::Symbol("get-request-path"), func);
		_env.scope()->put_func(lisp::Symbol("get-request-raw-path"), func);
		_env.scope()->put_func(lisp::Symbol("get-request-param"), func);
		_env.scope()->put_func(lisp::Symbol("get-request-header"), func);
		_env.scope()->put_func(lisp::Symbol("get-remote-host"), func);
		_env.scope()->put_func(lisp::Symbol("get-remote-port"), func);
		_env.scope()->put_func(lisp::Symbol("get-cookie"), func);
		_env.scope()->put_func(lisp::Symbol("get-all-cookies"), func);
	}

	void LispPage::applyResponse(HttpResponse & response) {
		class LispResponse : public lisp::Procedure {
		private:
			HttpResponse & response;
		public:
			LispResponse(HttpResponse & response)
				: response(response) {}
			virtual ~LispResponse() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				if (name->r_symbol() == "set-status-code") {
					int status = (int)lisp::eval(env, scope, iter.next())->r_integer().getInteger();
					string statusMessage;
					if (iter.avail()) {
						statusMessage = lisp::eval(env, scope, iter.next())->toPrintString();
					}
					if (statusMessage.empty()) {
						response.setStatus(status);
					} else {
						response.setStatus(status, statusMessage);
					}
					return HEAP_ALLOC(env, lisp::Integer(status));
				} else if (name->r_symbol() == "set-response-header") {
					string name = lisp::eval(env, scope, iter.next())->toPrintString();
					string value = lisp::eval(env, scope, iter.next())->toPrintString();
					response.setHeaderField(name, value);
					return HEAP_ALLOC(env, lisp::wrap_text(value));
				} else if (name->r_symbol() == "set-redirect") {
					string location = lisp::eval(env, scope, iter.next())->toPrintString();
					response.setRedirect(location);
					return HEAP_ALLOC(env, lisp::wrap_text(location));
				} else if (name->r_symbol() == "set-forward") {
					string location = lisp::eval(env, scope, iter.next())->toPrintString();
					response.setForward(location);
					return HEAP_ALLOC(env, lisp::wrap_text(location));
				} else if (name->r_symbol() == "set-file-transfer") {
					string path = lisp::eval(env, scope, iter.next())->toPrintString();
					response["set-file-transfer"] = path;
					return HEAP_ALLOC(env, lisp::wrap_text(path));
				} else if (name->r_symbol() == "push-set-cookie") {
					string cookie = lisp::eval(env, scope, iter.next())->toPrintString();
					vector<Cookie> cookies;
					cookies.push_back(Cookie(cookie));
					response.appendCookies(cookies);
					return HEAP_ALLOC(env, lisp::wrap_text(cookie));
				}
				return env.nil();
			}
		};
		_VAR func = HEAP_ALLOC(_env, new LispResponse(response));
		_env.scope()->put_func(lisp::Symbol("set-status-code"), func);
		_env.scope()->put_func(lisp::Symbol("set-response-header"), func);
		_env.scope()->put_func(lisp::Symbol("set-redirect"), func);
		_env.scope()->put_func(lisp::Symbol("set-forward"), func);
		_env.scope()->put_func(lisp::Symbol("set-file-transfer"), func);
		_env.scope()->put_func(lisp::Symbol("push-set-cookie"), func);
		
	}
	void LispPage::applyLoadPage() {
		class LispLoadPage : public lisp::Procedure {
		private:
		public:
			LispLoadPage() {}
			virtual ~LispLoadPage() {}
			LISP_PROCEDURE_PROC(env, scope, name, args) {
				Iterator<_VAR> iter = args.iter();
				FileStream reader(lisp::pathname(env, lisp::eval(env, scope, iter.next()))->r_pathname().file(), "rb");
				return HEAP_ALLOC(env, lisp::wrap_text(LispPage::parseLispPage(env, reader.readFullAsString())));
			}
		};
		_env.scope()->put_func(lisp::Symbol("load-page"), HEAP_ALLOC(_env, new LispLoadPage));
	}
    
	bool LispPage::compile(lisp::Env & env, const string & line) {
		try {
			lisp::compile(env, line);
			env.gc();
		} catch (lisp::ExitLispException e) {
			throw e;
		} catch (osl::Exception & e) {
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

	string LispPage::parseLispPage(lisp::Env & env, const string & src) {

		compile(env, "(defparameter *content* \"\")");
		
		lisp::BufferedCommandReader reader;
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
		} catch (lisp::ExitLispException e) {
			logger->debug("[LispPage - (quit)]");
		}

		return env.scope()->rget_var(lisp::Symbol("*content*"))->toPrintString();
	}
	
}
