module app.Document;

class Document(T): Object {
	private:
	
	T[]			history_;
	T			current_;
	char[]		filename_;
	static int	count_ = 0;
	
	this () {
		super ();
		++count;
	}
	
	~this () {
		foreach (ref o; history)
			delete o;
		delete history;
	}
	
	public:
	
	char[] filename () {
		return filename_;
	}
	
	void filename (char[] val) {
		filename_ = val;
	}
	
	int count () {
		return count_;
	}
	
	void count (int val) {
		count_ = val;
	}
	
	/**
	 * TODO: copy current_ and append the copy to history_
	 */
	void save () {
	}
	
	/**
	 * TODO: current_ should be saved with save () and then copy 
	 * history_.length - 2 into current_
	 */
	void undo () {
	}
	
	void redo () {
	}
}
