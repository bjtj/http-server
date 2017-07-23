#ifndef __LISP_PAGE_HPP__
#define __LISP_PAGE_HPP__

#include <liboslayer/Lisp.hpp>
#include <liboslayer/Library.hpp>
#include <liboslayer/AutoRef.hpp>
#include <string>
#include "HttpSession.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"
#include "HttpServerConfig.hpp"

namespace HTTP {
	
	class LispPage {
	private:
		LISP::Env _env;
	public:
		LispPage();
		virtual ~LispPage();
		LISP::Env & env();
		std::string toLispySymbolName(const std::string & name);
		void applyProperties(const std::map<std::string, std::string> & props);
		void applyWeb(HttpServerConfig & config);
		void applyAuth(HttpRequest & request, HttpResponse & response);
        void applySession(OS::AutoRef<HttpSession> session);
		void applyRequest(HttpRequest & request);
		void applyResponse(HttpResponse & response);
		void applyLoadPage();
		std::string parseLispPage(const std::string & src);
		
		static void applyWeb(LISP::Env & env, HttpServerConfig & config);
		static void applyAuth(LISP::Env & env, HttpRequest & request, HttpResponse & response);
        static void applySession(LISP::Env & env, OS::AutoRef<HttpSession> session);
		static void applyRequest(LISP::Env & env, HttpRequest & request);
		static void applyResponse(LISP::Env & env, HttpResponse & response);
		static void applyLoadPage(LISP::Env & env);
		static bool compile(LISP::Env & env, const std::string & line);
		static std::string convertLispPageToCode(const std::string & src);
		static std::string parseLispPage(LISP::Env & env, const std::string & src);
	};
}

#endif
