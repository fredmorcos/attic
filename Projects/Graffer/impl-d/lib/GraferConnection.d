module lib.GraferConnection;

import
	lib.Position;

class GraferConnection: Object {
private:
	Position*[]	points_;
	
public:
	Position*[] points () {
		return points;
	}
	
	void points (Position*[] val) {
		points_ = val;
	}
	
	void addPoint (Position* newPoint) {
		points_ ~= newPoint;
	}
}
