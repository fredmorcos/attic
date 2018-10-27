class BinaryTree
{
	Node root;
	
	public BinaryTree()
	{
		root = null;
	}
	
	// iterative insertion
	public void add(int key)
	{
		Node n = new Node(key);
		if(root == null)
			root = n;
		else
		{			
			Node current = root;
			while(key != current.key)
			{
				if(key < current.key)
				{
					if(current.left == null)
						current.left = n;
					else
						current = current.left;
				}
				else
				{
					if(current.right == null)
						current.right = n;
					else
						current = current.right;
				}
			}
		}
	}
	
	// recursive insertion
	public void addRec(int key)
	{
		if(root == null)
			root = new Node(key);
		else
			addRec(key, root);
	}
	
	public void addRec(int key, Node current)
	{	
		if(key < current.key)
			if(current.left == null)
				current.left = new Node(key);
			else
				addRec(key, current.left);
		else
			if(current.right == null)
				current.right = new Node(key);
			else
				addRec(key, current.right);
	}
	
	// Searching iteratively
	public Node find(int key)
	{
		Node current = root;		
		while(current != null)
		{
			if(key == current.key)
				return current;
			else if(key < current.key)
				current = current.left;
			else
				current = current.right;
		}		
		return null;
	}
	
	// Searching recursively
	public Node findRec(int key)
	{
		return findRec(key, root);
	}
	
	private Node findRec(int key, Node current)
	{
		if(current == null)
			return null;
		else if(key == current.key)
			return current;
		else if(key < current.key)
			return findRec(key, current.left);
		else
			return findRec(key, current.right);
	}
	
	// Prefix Traversal
	public String TraversePrefixAll()
	{
		return TraversePrefix(root);
	}
	
	public String TraversePrefix(Node current)
	{
		if(current == null)
			return "";
		else
			return (current.key + " " + TraversePrefix(current.left) 
					+ TraversePrefix(current.right));
	}
	
	// Postfix Traversal
	public String TraversePostfixAll()
	{
		return TraversePostfix(root);
	}
	
	public String TraversePostfix(Node current)
	{
		if(current == null)
			return "";
		else
			return (TraversePostfix(current.left) 
					+ TraversePostfix(current.right) + current.key + " ");
	}
	
	//Infix Traversal
	public String TraverseInfixAll()
	{
		return TraverseInfix(root);
	}
	
	public String TraverseInfix(Node current)
	{
		if(current == null)
			return "";
		else
			return (TraverseInfix(current.left) + current.key + " "
					+ TraverseInfix(current.right));
	}
	
	// deletion
	public boolean delete(int key)
	{
		if(root == null)
			return false;
		Node current = root;
		Node parent = root;
		boolean right = true;
		// searching for the node to be deleted
		while(key != current.key)
		{
			if(key < current.key)
			{
				right = false;
				parent = current;
				current = current.left;
			}
			else
			{
				right = true;
				parent = current;
				current = current.right;
			}
			if(current == null)
				return false;
		}
		
		Node substitute = null;
		// case 1: Node to be deleted has no children
		if(current.left == null && current.right == null)
			substitute = null;
		// case 2: Node to be deleted has one child
		else if(current.left == null)
			substitute = current.right;
		else if(current.right == null)
			substitute = current.left;
		// case 3: Node to be deleted has two children
		else
		{
			Node successor = current.right;
			Node successorParent = current;
			// searching for the inorder successor of the node to be deleted
			while(successor.left != null)
			{
				successorParent = successor;
				successor = successor.left;
			}
			substitute = successor;
			if(successorParent == current)
			{
				if(successor.right == null)
					successorParent.right = null;
				else
					successorParent.right = successor.right;
			}
			else
			{
				if(successor.right == null)
					successorParent.left = null;
				else
					successorParent.left = successor.right;
			}
			successor.right = current.right;
			successor.left = current.left;
			substitute = successor;
		}
		
		if(current == root)
			root = substitute;
		else if(right)
			parent.right = substitute;
		else
			parent.left = substitute;
		return true;
	}
}
