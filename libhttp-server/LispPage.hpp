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
		void applyWeb();
		void applyWeb(LISP::Env & env);
		void applySession(HttpSession & session);
		void applySession(LISP::Env & env, HttpSession & session);
		void applyRequest(HttpRequest & request);
		void applyRequest(LISP::Env & env, HttpRequest & request);
		void applyResponse(HttpResponse & response);
		void applyResponse(LISP::Env & env, HttpResponse & response);
		bool eval(LISP::Var & var, LISP::Env & env);
		bool compile(const std::string & line, LISP::Env & env);
		std::string parseLispPage(const std::string & src);
		std::string parseLispPage(LISP::Env & env, const std::string & src);
	};
}

#endif
