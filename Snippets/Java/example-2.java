public class ex2 {
	void printRange(BinaryTree x,int lower,int upper) {
		printRange(x.root,lower,upper);
	}
	
	//assuming the keys dont have to be printed in order
	void printRange(Node x,int lower,int upper) {
		printRange(x.left,lower,upper);
		if(x.key>=lower && x.key<=upper) {
			System.out.print(x.key);
		}
		printRange(x.right,lower,upper);
	}
}