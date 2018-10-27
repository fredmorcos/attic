//package Triominos.Engine;
public class GameBoard {
	Slot[][] board=new Slot[8][7];
	boolean flagUp=false;
	boolean flagDown=false;
	boolean flagRight=false;
	boolean flagLeft=false;
	
	public GameBoard() {
		
	}
	
	public boolean insertTile(Tile x, int r, int c) {
		if(board[r][c].tile!=null) {
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
						if(board[r-1][c].tile==null){
							flagUp=true;
						}
						else{
							if(board[r-1][c].tile.a==x.a && board[r-1][c].tile.c==x.c){
								flagUp=true;
							}//check the base of the upper tile (if exists)
						}
					}
					if(checkDownTile(r,c)) {
						if(board[r+1][c].tile==null) {
							flagDown=true;
						}
						else{
							if(board[r+1][c].tile.b==x.b) {
							flagDown=true;
							}	//check the tip of the lower tile
						}
					}						
					if(checkLeftTile(r,c)) {
						if(board[r][c-1].tile==null){
							flagLeft=true;
						}
						else{
							if(board[r][c-1].tile.b==x.a && board[r][c-1].tile.c==x.b) {
								flagLeft=true;	
							}
						}	//check the left tile (reversed tiles)
					}
					if(checkRightTile(r,c)) {
						if(board[r][c+1].tile==null){
							flagRight=true;
						}
						else{
							if(board[r][c+1].tile.b==x.c && board[r][c+1].tile.a==x.b){
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
						if(board[r-1][c].tile==null){
							flagUp=true;
						}
						else{
							if(board[r-1][c].tile.b==x.b){
								flagUp=true;
							}//check the base of the upper tile (if exists)
						}
					}
					if(checkDownTile(r,c)) {
						if(board[r+1][c].tile==null) {
							flagDown=true;
						}
						else{
						if(board[r+1][c].tile.a==x.a && board[r+1][c].tile.c==x.c) {
							flagDown=true;
							}	//check the tip of the lower tile
						}
					}
					if(checkLeftTile(r,c)) {
						if(board[r][c-1].tile==null){
							flagLeft=true;
						}
						else{
							if(board[r][c-1].tile.c==x.b && board[r][c-1].tile.b==x.a)  {
								flagLeft=true;	
							}
						}	//check the left tile (reversed tiles)
					}
					if(checkRightTile(r,c)) {
						if(board[r][c+1].tile==null){
							flagRight=true;
						}
						else{
							if(board[r][c+1].tile.a==x.b && board[r][c+1].tile.b==x.c){
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
		for(int r=0; r<8; r++) {
			for(int c=0; c<7; c++) {
				if(board[r][c].tile==null) {
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
			if(board[r-1][c].tile==null){
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
			if(board[r+1][c].tile==null) {
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
			if(board[r][c-1].tile==null) {
				return false;
			}
			else {
				return true;
			}
		}
	}
	
	private boolean checkRightTile(int r, int c) {
		if(c==7) {
			return false;
		}
		else {
			return true;
		}
	}
	
	
	
	
	
	
/*   	public boolean upperCenterHexagon() {
   		Tile current=p.myBoard.getTile();
   		try {  
   			if(board[r][c-1].current!=null&&board[r+1][c-1].current!=null&&board[r+1][c].current!=null&&board[r+1][c+1].current!=null&& board[r][c+1].current!=null )
   				return true;
   			else
   				return false;
   		}
   		catch (Exception e){
   			return false;
   		}
   	}
   
   
   	public boolean upperRightHexagon () {
   		try{
   			Tile current=p.myBoard.getTile();
   	if (board[r][c-1].current!=null&&board[r][c-2].current!=null&&board[r+1][c-2].current!=null&&board[r+1][c-1].current!=null&& board[r+1][c].current!=null )
   	return true;
   	
   	else return false;
   	
   } catch (Exception e){
   	return false;
   }
   
   
   public boolean upperLeftHexagon ()
   {
   	try {
   	
   	Tile current=p.myBoard.getTile();
   	if (board[r+1][c].current!=null&&board[r+1][c+1].current!=null&&board[r+1][c+2].current!=null&&board[r][c+2].current!=null&& board[r][c+1].current!=null )
   	return true;
   	
   	else return false;
   }
   catch (Exception e){
   	return false;
   }
   
   public boolean lowerLeftHexagon()
   {
   	try {
   	
   	Tile current=p.myBoard.getTile();
   	if (board[r][c+1].current!=null&&board[r][c+2].current!=null&&board[r-1][c+2].current!=null&&board[r-1][c+1].current!=null&& board[r-1][c].current!=null )
   	return true;
   	
   	else return false;
   }catch (Exception e){
   	return false;
   }
   
    public boolean lowerCenterHexagon()
   {
   	try {
   	
   	Tile current=p.myBoard.getTile();
   	if (board[r][c+1].current!=null&&board[r][c-1].current!=null&&board[r-1][c+1].current!=null&&board[r-1][c].current!=null&& board[r-1][c-1].current!=null )
   	return true;
   	
   	else return false;
   }catch (Exception e){
   	return false;
   }
   
    public boolean lowerRightHexagon()
   {
   	try {
   	
   	Tile current=p.myBoard.getTile();
   	if (board[r-1][c].current!=null&&board[r-1][c-1].current!=null&&board[r-1][c-2].current!=null&&board[r][c-2].current!=null&& board[r][c-1].current!=null )
   	return true;
   	
   	else return false;
   }catch (Exception e){
   	return false;
   }
   
   
   
   
   
public boolean bridge ()
{Tile current=p.myBoard.getTile();
   try {
if (!current.tipUp)
    {
    
         	if (current.a==board[r+1][c].current.a && board[r+2][c].current!=null)//// the new tile is Tile, and is upper
            return true;
            }catch (Exception e){
   		
   		return false;
   	}
     else if (current.b==board[r-1][c].current.b && current.c==board[r-1][c].current.c && board[r-2][c].current!=null)/////it is lower
   	 return true;
	}
	(Exception e){
   		
   		return false;
   	}
else
	if (current.a==board[r-1][c].a && board[r+1][c].current!=null)                                             /////in the middle
	return true;
	
	
	else return false;
	(Exception e){
   		
   		return false;
   	}
	
	}*/
}