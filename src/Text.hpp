#ifndef __TEXT_HPP__
#define __TEXT_HPP__

#include <string>
#include <vector>
#include <map>
#include <utility>

namespace UTIL {

	class Text {
	private:
		Text();
		virtual ~Text();
		Text(const Text&);
		Text & operator=(const Text&);
	public:
		static std::string trim(std::string str);
		static bool match(std::string f, std::string s);
		static std::vector<std::string> split(std::string target, std::string sep);
		static std::string join(std::vector<std::string> & vec, std::string glue);
		static bool contains(std::vector<std::string> & vec, std::string target);
		static std::string replaceAll(std::string src, std::string match, std::string rep);
		static std::string quote(std::string str, std::string q = "'");
		static std::string toMapString(std::map<std::string, std::string> & m,
									   std::string item_sep = ": ",
									   std::string line_sep = ", ");
		static std::string toMapString(std::vector<std::pair<std::string, std::string> > & m,
									   std::string item_sep = ": ",
									   std::string line_sep = ", ");
		static int toInt(std::string str);
		static std::string toString(int i);
		static bool startsWith(std::string a, std::string b, bool ignorecase=false);
		static bool endsWith(std::string a, std::string b, bool ignorecase=false);
		static int compareIgnoreCase(std::string a, std::string b);
		static bool equalsIgnoreCase(std::string a, std::string b);
	};
}

#endif
