#ifndef __LISP_PAGE_HPP__
#define __LISP_PAGE_HPP__

#include <liboslayer/Lisp.hpp>
#include <string>
#include "HttpSession.hpp"
#include "HttpRequest.hpp"
#include "HttpResponse.hpp"

namespace HTTP {
	
	class LispPage {
	private:
		LISP::Env global_env;
	public:
		LispPage();
		virtual ~LispPage();
		LISP::Env & env();
		std::string toLispySymbolName(const std::string & name);
		void applyProperties(const std::map<std::string, std::string> & props);
		void applyWeb();
		void applyAuth(HttpRequest & request, HttpResponse & response);
		void applySession(HttpSession & session);
		void applyRequest(HttpRequest & request);
		void applyResponse(HttpResponse & response);
		void applyLoadPage();
		std::string parseLispPage(const std::string & src);
		
		static void applyWeb(LISP::Env & env);
		static void applyAuth(LISP::Env & env, HttpRequest & request, HttpResponse & response);
		static void applySession(LISP::Env & env, HttpSession & session);
		static void applyRequest(LISP::Env & env, HttpRequest & request);
		static void applyResponse(LISP::Env & env, HttpResponse & response);
		static void applyLoadPage(LISP::Env & env);
		static bool compile(const std::string & line, LISP::Env & env);
		static std::string parseLispPage(LISP::Env & env, const std::string & src);
	};
}

#endif
