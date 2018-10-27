module List;

struct ListItem(T) {
	ListItem!(T)* next, previous;
	T data;
}

struct List(T) {
	ListItem!(T)* first, last, nth;

	void append(T t) {
		if (first == null) {
			first = new ListItem!(T);
			first.data = t;
			last = first;
			nth = first;
		}
		else {
			last.next = new ListItem!(T);
			last.next.data = t;
			last.next.previous = last;
			last = last.next;
		}
	}

	void prepend(T t) {
		if (first == null) {
			first = new ListItem!(T);
			first.data = t;
			last = first;
			nth = first;
		}
		else {
			first.previous = new ListItem!(T);
			first.previous.data = t;
			first.previous.next = first;
			first = first.previous;
		}
	}

	List!(T)* getSubList() {
		List!(T)* l = new List!(T);
		l.first = nth.next;
		l.last = last;
		l.nth = l.first;
		return l;
	}

	void goToFirst() {
		nth = first;
	}

	void goToLast() {
		nth = last;
	}

	T getFirst() {
		return first.data;
	}

	T getLast() {
		return last.data;
	}

	bool next() {
		if (nth.next) {
			nth = nth.next;
			return true;
		}
		else
			return false;
	}

	bool previous() {
		if (nth.previous) {
			nth = nth.previous;
			return true;
		}
		else
			return false;
	}

	T getCurrent() {
		return nth.data;
	}
}

