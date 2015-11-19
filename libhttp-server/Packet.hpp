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
        size_t size;
        size_t pos;
        size_t limit;
        
    private:
        Packet(char * buffer, size_t size);
        
    public:
        Packet(size_t size);
        Packet(const Packet & other);
        virtual ~Packet();
        
        static Packet wrap(char * buffer, size_t len);
        
        void clear();
        size_t remaining();
        void write(const char * data, size_t len);
        char * getData();
        size_t getLength();
        void setPosition(size_t position);
        size_t getLimit();
        void resetLimit();
        void setLimit(size_t limit);
    };
    
    
}

#endif