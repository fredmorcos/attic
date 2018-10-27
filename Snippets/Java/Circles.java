/* import it in so we can use graphics */
import java.awt.*;
/* import this so we can get mouse clicks */
import java.awt.event.*;
/* import for events and actions */
import javax.swing.*;

/* extends jframe to draw on and action listener to catch the clicks */
public class circles extends JFrame {
	/* button to draw on */
	circlesButton button;

	/* circles constructor */
	public circles () {
		/* call the jframe constructor */
		super ();
		/* to close the program when the window closes */
		setDefaultCloseOperation (EXIT_ON_CLOSE);

		/* create a new button with circles */
		button = new circlesButton ();
		button.setBounds (10, 10, 200, 200);
		button.setVisible (true);

		/* set some properties for our jframe
			and add the button to it */
		setBounds (10, 10, 200, 200);
		getContentPane().add (button);
		setVisible (true);
	}

	/* our main method :) */
	public static void main (String args[]) {
		circles x = new circles ();
	}
}

class circlesButton extends JButton implements MouseListener {
	/* left circle */
	int lcx, lcy, lcw, lch;
	public Color lc;

	/* right circle */
	int rcx, rcy, rcw, rch;
	public Color rc;

	public circlesButton () {
		/* call the jbutton constructor */
		super ("");

		/* set the coordinates of the circles */
		lcx = 20; lcy = 20; lcw = 80; lch = 80;
		lc = Color.blue;

		rcx = 80; rcy = 20; rcw = 80; rch = 80;
		rc = Color.black;

		addMouseListener (this);
	}

	/* over-write the button's paint() method so everytime we move,
		resize or minimize the window, the circles get repainted */
	public void paint (Graphics g) {
		/* create an ellipse, (x, y, w, h) */
		g.setColor (lc);
		g.fillOval (lcx, lcy, lcw, lch);
		g.setColor (rc);
		g.fillOval (rcx, rcy, rcw, rch);
	}

	/* overwrite the mouseClicked method from the MouseListener
		because we need to get WHERE the mouse was clicked */
	public void mouseClicked(MouseEvent e) {
		/* if click is inside left and right circles (in the middle)
			and inside the X and Y positions too */
		if (e.getX() <= (lcx + lcw) && e.getX() >= rcx
			&& e.getY() >= lcy && e.getY() <= (lcy + lch)) {
			System.out.println ("In the middle!");
			lc = Color.white;
			rc = Color.pink;
		}
		/* else if, then check if it was clicked in the left circle */
		else if (e.getX() >= lcx && e.getX() <= (lcx + lcw)
				&& e.getY() >= lcy && e.getY() <= (lcy + lch)) {
			System.out.println ("In the LEFT circle!!!");
			lc = Color.yellow;
		}
		/* else if, then check if it was clicked in the right circle */
		else if (e.getX() >= rcx && e.getX() <= (rcx + rcw)
				&& e.getY() >= rcy && e.getY() <= (rcy + rch)) {
			System.out.println ("In the RIGHT circle!!!");
			rc = Color.red;
		}
		/* else it was clicked out of both circles */
		else {
			System.out.println ("Out!");
		}
	}

	/* other methods to overwrite that we will not use, so they are
		empty */
	public void mouseExited(MouseEvent e) {}
	public void mouseEntered(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
}
