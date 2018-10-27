/*
 * Main.java
 *
 * Created on October 29, 2007, 12:08 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

/**
 *
 * @author Drew
 */

import java.util.*;
public class Main {
	
	/** Creates a new instance of Main */
	public Main() 
	{
		Alarm a = new Alarm();
		AlarmEvent x1 = new AlarmEvent(new GregorianCalendar(2007, 9, 29, 14, 17), "HELLO", "WAKE UP FREEEEEEEED");
		x1.addActionListener(new AlarmEventActionListener());
		a.addEvent(x1);
		AlarmEvent x2 = new AlarmEvent(new GregorianCalendar(2007, 9, 29, 14, 16), "HELLO", "WAKE UP FREEEEEEEEDssssssssss");
		x2.addActionListener(new AlarmEventActionListener());
		a.addEvent(x2);
		a.start();
	}

	public static void main (String [] args)
	{
		Main x = new Main();
	}

}
