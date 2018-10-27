module lib.Position;

struct Position {
private:
	int	x_,
		y_;
		
public:	
	/* Get and Set */
	
	int x () {
		return x_;
	}
	
	void x (int val) {
		x_ = val;
	}
	
	int y () {
		return y_;
	}
	
	void y (int val) {
		y_ = val;
	}
}
