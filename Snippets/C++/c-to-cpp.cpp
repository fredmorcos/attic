#include <iostream>		// cout, endl
#include <vector>

using namespace std;

const unsigned int noe = 40;	// Number Of Elements

inline void sum (int& acc, vector<int> data)
{
  // Initialize `acc` to 0, to avoid errors if it has any other value
  acc = 0;

  // Go over each element in `data` and accumulate the sum into `acc`
  for (unsigned int i = 0; i < data.size(); ++i)
    acc += data[i];
}

int main ()
{
  int acc = 0;			// Accumulator
  vector<int> data(noe);	// Vector of ints of size `noe`

  // Initialize the vector with values
  for (unsigned int i = 0; i < data.size(); ++i)
    data[i] = i;

  // Sum all values in `data` into `acc`
  sum(acc, data);

  cout << "sum is " << acc << endl;

  return 0;
}
