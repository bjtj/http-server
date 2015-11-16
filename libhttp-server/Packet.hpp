#ifndef __PACKET_HPP__
#define __PACKET_HPP__

#include <liboslayer/os.hpp>

namespace HTTP {

	/**
	 * @brief packet
	 */
	class Packet {
	private:
		char * buffer;
		int _size;
		int _length;

	public:
		Packet();
		Packet(int size);
		Packet(char * buffer, int size);
		Packet(const Packet & other);
		virtual ~Packet();

		void clear();
		char * getBuffer();
		int put(char * data, int len);
		int size();
		void resize(int size);
		int length();

		Packet & operator= (const Packet & other);
	};

}

#endif