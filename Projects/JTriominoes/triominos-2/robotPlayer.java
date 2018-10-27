import java.awt.*;
import javax.swing.*;

public class robotPlayer extends player {
	public robotPlayer() {
		super();
	}
	
	public robotPlayer(String n, int pn, Color pc) {
		super(n, pn, pc, null);
	}
	
	public void findBestPlayer() {
		//best play
	}
	
	public static void main(String args[]) {
		robotPlayer x=new robotPlayer("Computer Player 1", 2, Color.GREEN);
		System.out.println(x);
	}
}
