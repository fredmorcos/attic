import javax.swing.*;
import java.awt.*;

public class PhotoChoosing {
	public static void main(String args[]) {
		//JFileChooser x=new JFileChooser();
		//int d=x.showOpenDialog(null);
		//String c=x.getSelectedFile().getAbsolutePath();
		//System.out.println(c);
		JColorChooser n=new JColorChooser();
		Color g=n.showDialog(null, "boo", Color.GREEN);
		System.out.println(g.toString());
		
	}
}