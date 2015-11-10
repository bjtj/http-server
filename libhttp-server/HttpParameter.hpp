#ifndef __HTTP_PARAMETER_HPP__
#define __HTTP_PARAMETER_HPP__

#include <string>
#include <utility>
#include <vector>
#include <map>
#include <cstdlib>

namespace HTTP {

	/**
	 * @brief http header field
	 */
	class HttpParameter {
	private:
		std::string name;
		std::vector<std::string> values;
	public:
		HttpParameter();
		HttpParameter(const std::string & name);
		HttpParameter(const std::string & name, const std::string & value);
		virtual ~HttpParameter();
		
		bool empty();
		size_t size();
		std::string & getName();
		void setName(std::string name);
		std::string getFirstValue();
		std::vector<std::string> & getValues();
		std::string & getValue(int index);
		void setValue(std::string value);
		void setValues(std::vector<std::string> & values);
		void appendValue(std::string value);
		void appendValues(std::vector<std::string> & values);

		std::string & operator[](int index);
		virtual std::string toString();
	};

}

#endif
