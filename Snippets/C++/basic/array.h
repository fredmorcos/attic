using namespace std;

class Array
{
	private:
		int size, *ptr;

	public:
		Array (int = 5);
		Array (Array &);
		void resize (int);
		void print ();

		int &at (int);

		~Array ();
};
