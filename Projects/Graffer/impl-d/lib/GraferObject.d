module GraferObject;

import
	lib.Position,
	lib.Size;
	
class GraferObject: Object {
private:
	Position	position_;
	Size		size_;
	int			rotation_;
	char[]		file_;
	
public:	
	/* Get and Set */
	
	Position position () {
		return position_;
	}
	
	void position (Position val) {
		position_ = val;
	}
	
	Size size () {
		return size_;
	}
	
	void size (Size val) {
		size_ = val;
	}
	
	int rotation () {
		return rotation_;
	}
	
	void rotation (int val) {
		rotation_ = val;
	}
	
	char[] file () {
		return file_;
	}
	
	void file (char[] val) {
		file_ = val;
	}
}
