public class ex1 {
	public int maxDepth(BinaryTree x) {
		if(x.root.left==null && x.root.right==null) {
			return 0;
		}
		else {
			int l=0,r=0;
			l=maxNodeDepth(x.root.left);
			r=maxNodeDepth(x.root.right);
			if(l>=r) {
				return l;
			}
			else {
				return r;
			}
		}
	}
	
	public int maxNodeDepth(Node x) {
		if(x==null) {
			return 0; 
		}
		else {
			int l=0,r=0;
			l=maxNodeDepth(x.left);
			r=maxNodeDepth(x.right);
			if(l>=r) {
				return 1+l;
			}
			else {
				return 1+r;
			}
		}
	}
}