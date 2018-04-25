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

namespace http {
	
	class LispPage {
	private:
		lisp::Env _env;
        std::string _last_command;
	public:
		LispPage();
		virtual ~LispPage();
		lisp::Env & env();
		std::string toLispySymbolName(const std::string & name);
		void applyProperties(const std::map<std::string, std::string> & props);
		void applyWeb(HttpServerConfig & config);
		void applyAuth(HttpRequest & request, HttpResponse & response);
        void applySession(HttpRequest & request, osl::AutoRef<HttpSession> session);
		void applyRequest(HttpRequest & request);
		void applyResponse(HttpResponse & response);
		void applyLoadPage();
		std::string parseLispPage(const std::string & src);

		static bool compile(lisp::Env & env, const std::string & line);
		static std::string convertLispPageToCode(const std::string & src);
		static std::string parseLispPage(lisp::Env & env, const std::string & src);
	};
}

#endif
