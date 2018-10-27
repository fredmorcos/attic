/* Semaphore Implementation */

public class semaphore {
	/* semaphore's number */
	protected int count;

	/*	semaphore constructor:
			will check if the semaphore is initialized to something less
			than zero. if so, will initialize to zero, else, will keep.
	
		@c:	temporary variable to do initialization, count will be set to c
	*/
	public semaphore (int c) {
		if (c < 0) {
			System.out.println ("\tSEM\t\t\tsem\t\t\tCount < 0, has been set to 0.");
			count = 0;
		}
		else {
			count = c;
		}
	}

	/*	default semaphore constructor:
			initializes the semaphore to zero by default.
	*/
	public semaphore () {
		count = 0;
	}

	/*	semaphore's down function:
			if the semaphore is already 0, will let the current wait until
			it gets to one, else, normally decrement the semaphore (count).
	*/
	public synchronized void down () throws InterruptedException {
			while (count == 0) {
				/* will wait the current thread */
				wait ();
			}
			count--;
	}

	/*	semaphore's up function:
			increment the semaphore and notify a waiting thread. we don't
			need to check if the semaphore reaches a maximum value because
			we don't care about the number of threads that are calling
			down().
	*/
	public synchronized void up () {
		count++;
		notify ();
	}
}
