#include "utils.hpp"
#include <libhttp-server/Url.hpp>
#include <liboslayer/Logger.hpp>
#include <libhttp-server/UrlEncoderDecoder.hpp>

using namespace std;
using namespace HTTP;
using namespace UTIL;

static void test_url() {

	string text = "http://10.0.12.96:2869/upnphost/udhisapi.dll?content=uuid:27151292-513d-4dcd-912a-a2b8cdc0a128";
	
	Url url(text);

	ASSERT(url.toString(), ==, text);
	ASSERT(url.getPath(), ==, "/upnphost/udhisapi.dll");
	ASSERT(url.getPathAndQuery(), ==, "/upnphost/udhisapi.dll?content=uuid:27151292-513d-4dcd-912a-a2b8cdc0a128");
	ASSERT(url.getHost(), ==, "10.0.12.96");
	ASSERT(url.getPort(), ==, "2869");
	ASSERT(url.getScheme(), ==, "http");

	url = Url("http://localhost/");

	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "80");
	ASSERT(url.getScheme(), ==, "http");

	url = Url("https://localhost/");

	ASSERT(url.getScheme(), ==, "https");
	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "443");

	url = Url("http://username:password@localhost");

	ASSERT(url.getScheme(), ==, "http");
	ASSERT(url.getUsername(), ==, "username");
	ASSERT(url.getPassword(), ==, "password");
	ASSERT(url.getHost(), ==, "localhost");
	ASSERT(url.getPort(), ==, "80");
	ASSERT(url.toString(), ==, "http://username:password@localhost:80/");

	// validate
	try {
		Url::validateUrlFormat("");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.toString().size(), >, 0);
	}

	try {
		Url::validateUrlFormat("//");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.toString().size(), >, 0);
	}

	try {
		Url::validateUrlFormat("/home/user/");
		throw "It should not be thrown!";
	} catch (UrlParseException & e) {
		ASSERT(e.toString().size(), >, 0);
	}

	Url::validateUrlFormat("file:///home/user/");
}

static void test_url_tostring() {
	string u = "http://localhost:9000/page.html?a=A&b=B&c=C";
	Url url(u);
	ASSERT(url.toString(), ==, u);
}

static void test_file_url() {
	Url url("file:///test.txt");
	ASSERT(url.getPath(), ==, "/test.txt");
}

static void test_url_encode() {
	string sample =
"  1 Raw_Read_Error_Rate     0x002f   100   100   051    Pre-fail  Always       -       1\n"
"  2 Throughput_Performance  0x0026   252   252   000    Old_age   Always       -       0\n"
"  3 Spin_Up_Time            0x0023   092   092   025    Pre-fail  Always       -       2573\n"
"  4 Start_Stop_Count        0x0032   100   100   000    Old_age   Always       -       182\n"
"  5 Reallocated_Sector_Ct   0x0033   252   252   010    Pre-fail  Always       -       0\n"
"  7 Seek_Error_Rate         0x002e   252   252   051    Old_age   Always       -       0\n"
"  8 Seek_Time_Performance   0x0024   252   252   015    Old_age   Offline      -       0\n"
"  9 Power_On_Hours          0x0032   100   100   000    Old_age   Always       -       318\n"
" 10 Spin_Retry_Count        0x0032   252   252   051    Old_age   Always       -       0\n"
" 11 Calibration_Retry_Count 0x0032   100   100   000    Old_age   Always       -       1\n"
" 12 Power_Cycle_Count       0x0032   100   100   000    Old_age   Always       -       397\n"
"191 G-Sense_Error_Rate      0x0022   100   100   000    Old_age   Always       -       1\n"
"192 Power-Off_Retract_Count 0x0022   100   100   000    Old_age   Always       -       316\n"
"194 Temperature_Celsius     0x0002   035   060   000    Old_age   Always       -       35 (0 21 0 0 0)\n"
"195 Hardware_ECC_Recovered  0x003a   100   100   000    Old_age   Always       -       0\n"
"196 Reallocated_Event_Count 0x0032   252   252   000    Old_age   Always       -       0\n"
"197 Current_Pending_Sector  0x0032   252   252   000    Old_age   Always       -       0\n"
"198 Offline_Uncorrectable   0x0030   252   252   000    Old_age   Offline      -       0\n"
"199 UDMA_CRC_Error_Count    0x0036   200   200   000    Old_age   Always       -       0\n"
"200 Multi_Zone_Error_Rate   0x002a   100   100   000    Old_age   Always       -       30\n"
"223 Load_Retry_Count        0x0032   100   100   000    Old_age   Always       -       1\n"
"225 Load_Cycle_Count        0x0032   100   100   000    Old_age   Always       -       83";

	ASSERT(UrlDecoder::decode(UrlEncoder::encode(sample)), ==, sample);
	ASSERT(UrlDecoder::decode_plus(UrlEncoder::encode_plus(sample)), ==, sample);
}

int main(int argc, char *args[]) {

	LoggerProfile profile("*");
	profile.allFormatters("basic");
	profile.allWriters("console");
	LoggerFactory::instance().setProfile(profile);

	Url url;

	test_url();
	test_url_tostring();
	test_file_url();
	test_url_encode();
    
    return 0;
}
