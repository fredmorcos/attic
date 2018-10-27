public class gameBoard {
	slot board[][]=new slot[23][9];
	boolean flagUp=false;
	boolean flagDown=false;
	boolean flagRight=false;
	boolean flagLeft=false;
	
	public gameBoard() {}
	
	public boolean insertTile(tile x, int r, int c) {
		if(board[r][c].slotTile!=null) {
			System.out.println("there is a tile already");
			return false;
		}
		else {
			if((r+c)%2==0) {
				if(x.tipUp==true) {
					System.out.println("tile cant fit because the slot tip is down and the tile tip is up");
					return false;
				}
				else {
					if(checkUpperTile(r,c)) {
						if(board[r-1][c].slotTile==null){
							flagUp=true;
						}
						else{
							if(board[r-1][c].slotTile.getA()==x.getA() && board[r-1][c].slotTile.getC()==x.getC()){
								flagUp=true;
							}//check the base of the upper tile (if exists)
						}
					}
					if(checkDownTile(r,c)) {
						if(board[r+1][c].slotTile==null) {
							flagDown=true;
						}
						else{
							if(board[r+1][c].slotTile.getB()==x.getB()) {
							flagDown=true;
							}	//check the tip of the lower tile
						}
					}						
					if(checkLeftTile(r,c)) {
						if(board[r][c-1].slotTile==null){
							flagLeft=true;
						}
						else{
							if(board[r][c-1].slotTile.getB()==x.getA() && board[r][c-1].slotTile.getC()==x.getB()) {
								flagLeft=true;	
							}
						}	//check the left tile (reversed tiles)
					}
					if(checkRightTile(r,c)) {
						if(board[r][c+1].slotTile==null){
							flagRight=true;
						}
						else{
							if(board[r][c+1].slotTile.getB()==x.getC() && board[r][c+1].slotTile.getA()==x.getB()){
								flagRight=true;
							}
						}
					}
					if(flagUp && flagDown && flagRight && flagLeft){
						board[r][c].addTile(x);
						return true;
					}
					else{
						return false;
					}
				}
			}
			else {
				if(x.tipUp==false) {
					System.out.println("tile cant be inserted because is inverted...");
					return false;
				}
				else {
					if(checkUpperTile(r,c)) {
						if(board[r-1][c].slotTile==null){
							flagUp=true;
						}
						else{
							if(board[r-1][c].slotTile.getB()==x.getB()){
								flagUp=true;
							}//check the base of the upper tile (if exists)
						}
					}
					if(checkDownTile(r,c)) {
						if(board[r+1][c].slotTile==null) {
							flagDown=true;
						}
						else{
						if(board[r+1][c].slotTile.getA()==x.getA() && board[r+1][c].slotTile.getC()==x.getC()) {
							flagDown=true;
							}	//check the tip of the lower tile
						}
					}
					if(checkLeftTile(r,c)) {
						if(board[r][c-1].slotTile==null){
							flagLeft=true;
						}
						else{
							if(board[r][c-1].slotTile.getC()==x.getB() && board[r][c-1].slotTile.getB()==x.getA())  {
								flagLeft=true;	
							}
						}	//check the left tile (reversed tiles)
					}
					if(checkRightTile(r,c)) {
						if(board[r][c+1].slotTile==null){
							flagRight=true;
						}
						else{
							if(board[r][c+1].slotTile.getA()==x.getB() && board[r][c+1].slotTile.getB()==x.getC()){
								flagRight=true;
							}
						}
					}	
					if(flagUp && flagDown && flagRight && flagLeft){
						board[r][c].addTile(x);
						return true;
					}
					else {
						return false;
					}
				}
			}
		}
	}
	
	public boolean isFull() {
		for(int r=0; r<23; r++) {
			for(int c=0; c<9; c++) {
				if(board[r][c].slotTile==null) {
					return false;
				}
			}
		}
		return true;
	}
	
	private boolean checkUpperTile(int r, int c) {
		if(r==0) {
			return false;
		}
		else {
			if(board[r-1][c].slotTile==null){
				return false;
			}
			else {
				return true;
			}
		}
	}
	
	private boolean checkDownTile(int r, int c) {
		if(r==6) {
			return false;
		}
		else {
			if(board[r+1][c].slotTile==null) {
				return false;
			}
			else {
				return true;
			}
		}
	}
	
	private boolean checkLeftTile(int r, int c) {
		if(c==0) {
			return false;
		}
		else {
			if(board[r][c-1].slotTile==null) {
				return false;
			}
			else {
				return true;
			}
		}
	}
	
	private boolean checkRightTile(int r, int c) {
		if(c==9) {
			return false;
		}
		else {
			if(board[r][c+1].slotTile==null) {
				return false;
			}
			else{
				return true;
			}
		}
	}



	public boolean upperCenterHexagon(int r,int c) {
   		//tile current=p.myBoard.getTile();
   		try {  
   			if(board[r][c-1].slotTile!=null && board[r+1][c-1].slotTile!=null && board[r+1][c].slotTile!=null && board[r+1][c+1].slotTile!=null&& board[r][c+1].slotTile!=null )
   				return true;
   			else
   				return false;
   		}
   		catch (Exception e){
   			return false;
   		}
   	}
   
   
   	public boolean upperRightHexagon (int r,int c) {
   		try{
   		//	tile current=p.myBoard.getTile();
   			if (board[r][c-1].slotTile!=null&&board[r][c-2].slotTile!=null&&board[r+1][c-2].slotTile!=null&&board[r+1][c-1].slotTile!=null&& board[r+1][c].slotTile!=null )
   				return true;
   			else 
   				return false;
   		}
   		catch (Exception e){
   			return false;
   		}
	}  
   
   public  boolean upperLeftHexagon (int r,int c)
   {
   		try {
   		//	tile current=p.myBoard.getTile();
   			if (board[r+1][c].slotTile!=null&&board[r+1][c+1].slotTile!=null&&board[r+1][c+2].slotTile!=null&&board[r][c+2].slotTile!=null&& board[r][c+1].slotTile!=null )
   				return true;
   			else
   				 return false;
   		}
   		catch (Exception e){
   			return false;
   		}
   }
   public boolean lowerLeftHexagon(int r,int c)
   {
   		try {
   		//	tile current=p.myBoard.getTile();
   			if (board[r][c+1].slotTile!=null&&board[r][c+2].slotTile!=null&&board[r-1][c+2].slotTile!=null&&board[r-1][c+1].slotTile!=null&& board[r-1][c].slotTile!=null )
   				return true;
   			else 
   				return false;
   		}
   		catch (Exception e){
   			return false;
   		}
   }
   
    public boolean lowerCenterHexagon(int r,int c)
   {
   		try {
   		//	tile current=p.myBoard.getTile();
   				if (board[r][c+1].slotTile!=null&&board[r][c-1].slotTile!=null&&board[r-1][c+1].slotTile!=null&&board[r-1][c].slotTile!=null&& board[r-1][c-1].slotTile!=null )
   					return true;
   		else 
   			return false;
   		}
   		catch (Exception e){
   			return false;
   		}
   }
   
    public boolean lowerRightHexagon(int r,int c)
   {
   		try {
   	
   			//tile current=p.myBoard.getTile();
   				if (board[r-1][c].slotTile!=null&&board[r-1][c-1].slotTile!=null&&board[r-1][c-2].slotTile!=null&&board[r][c-2].slotTile!=null&& board[r][c-1].slotTile!=null )
   					return true;
   				else 
   					return false;
   			}
   		catch (Exception e){
   			return false;
   		}
   }
  
public boolean bridge(int r, int c)
{
	//tile current=p.myBoard.getTile();
   	try {
		if (!board[r][c].slotTile.tipUp)
		{
          	if (board[r][c].slotTile.getA()==board[r+1][c].slotTile.getA() && board[r+2][c].slotTile!=null)
          	//// the new tile is Tile, and is the upper tile
            	return true;
        
    
     	else 
     	if (board[r][c].slotTile.getB()==board[r-1][c].slotTile.getB() && board[r][c].slotTile.getC()==board[r-1][c].slotTile.getC() && board[r-2][c].slotTile!=null)
     	/////it is the lower
   	        return true;
     	
    
        else 
        if (board[r][c].slotTile.getA()==board[r-1][c].slotTile.getA() && board[r+1][c].slotTile!=null){
        /////in the middle
	    return true;}
	
	
	else return false;
}
	}
catch	(Exception e){
   		
   		return false;
   	}
	return false;
	}
}
