/*
 * AlarmEventActionListener.java
 *
 * Created on October 29, 2007, 2:09 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

/**
 *
 * @author Drew
 */
import java.util.*;
import java.awt.event.*;

public class AlarmEventActionListener implements ActionListener
{
	
	/** Creates a new instance of AlarmEventActionListener */
	public AlarmEventActionListener() 
	{
		
	}
	
	public void actionPerformed(ActionEvent e)
	{
		System.out.println(((AlarmEvent)e.getSource()).getMessage());
	}
	
}
