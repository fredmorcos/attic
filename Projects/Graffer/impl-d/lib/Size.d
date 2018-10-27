module lib.Size;

struct Size {
private:
	int	width_,
		height_;
		
public:
	/* Get and Set */
	
	int width () {
		return width_;
	}
	
	void width (int val) {
		width_ = val;
	}
	
	int height () {
		return height_;
	}
	
	void height (int val) {
		height_ = val;
	}
}
