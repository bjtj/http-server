#include <liboslayer/os.hpp>
#include <liboslayer/Text.hpp>
#include <libhttp-server/HttpSession.hpp>
#include <libhttp-server/HttpSessionManager.hpp>

using namespace std;
using namespace OS;
using namespace HTTP;
using namespace UTIL;

size_t readline(char * buffer, size_t max) {
	if (fgets(buffer, (int)max - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return strlen(buffer);
	}
	return 0;
}

void s_print_sessions(HttpSessionManager & manager) {
	vector<HttpSession*> & sessions = manager.getSessions();
	printf("Sessions: %lu\n", sessions.size());
	for (vector<HttpSession*>::iterator iter = sessions.begin();
		 iter != sessions.end(); iter++) {
		printf(" - SESSION: %lu (remaining: %lu ms.)\n",
			   (*iter)->getId(), (*iter)->remainingLife());
	}
}

int main(int argc, char *args[]) {

	HttpSessionManager manager(10 * 1000);

	while (1) {
		
		char buffer[1024] = {0,};
		printf("> ");
		if (readline(buffer, sizeof(buffer)) > 0) {
			
			if (!strcmp(buffer, "q")) {
				break;
			}
			
			if (!strcmp(buffer, "c")) {
				HttpSession & session = manager.createSession();
				printf("Session created: %lu\n", session.getId());
			}

			if (!strcmp(buffer, "r")) {
				printf("Refresh\n");
				printf("-------\n");
				s_print_sessions(manager);
				printf("id: ");
				readline(buffer, sizeof(buffer));
				int id = Text::toInt(buffer);
				HttpSession & session = manager.getSession(id);
				session.updateLastAccessTime();
			}
			
			if (!strcmp(buffer, "d")) {
				printf("Destroy\n");
				printf("-------\n");
				s_print_sessions(manager);
				printf("id: ");
				readline(buffer, sizeof(buffer));
				int id = Text::toInt(buffer);
				manager.destroySession(id);
			}
			
		} else {
			s_print_sessions(manager);
			manager.removeOutdatedSessions();
		}
	}
    
    return 0;
}
