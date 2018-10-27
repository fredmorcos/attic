module lib.Grid;

struct Grid {
private:
	int	size_;

public:
	int size () {
		return size_;
	}

	void size (int val) {
		size_ = val;
	}
}

