#include <string>
using std::string;

class messageObject
{
public:
	messageObject(string);
	void displayMessage(void);

private:
	string message;
};
