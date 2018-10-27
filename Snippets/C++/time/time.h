#ifndef TIME_H
#define TIME_H

class time
{
	public:
		time();
		void set_time(	unsigned short int, 
				unsigned short int, 
				unsigned short int);
		void print_univ();
		void print_stan();

	private:
		unsigned short int hour, minute, second;
};

#endif
