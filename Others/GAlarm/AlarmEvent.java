
/*
 * AlarmEvent.java
 *
 * Created on October 21, 2007, 4:40 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

/**
 *
 * @author drew
 */

import java.util.*;
import java.awt.event.*;		
public class AlarmEvent implements Comparable
{
	Calendar date;
	String title, message;
	Vector<ActionListener> actionListeners;
	//Ringtone file path.

	/** Creates a new instance of AlarmEvent */
	public AlarmEvent(Calendar date, String title, String message)
	{
		this.date = date;
		this.title = title;
		this.message = message;
		actionListeners = new Vector<ActionListener>();
	}

	public Calendar getDate()
	{
		return date;
	}

	public void setDate(Calendar date)
	{
		this.date = date;
	}

	public String getTitle()
	{
		return title;
	}
	
	public void setTitle(String title)
	{
		this.title = title;
	}
	
	public String getMessage()
	{
		return message;
	}
	
	public void setMessage(String message)
	{
		this.message = message;
	}
	
	public void addActionListener(ActionListener a)
	{
		this.actionListeners.add(a);
	}
	
	public void removeActionListener(ActionListener a)
	{
		this.actionListeners.remove(a);
	}
	
	public void trigger()
	{
		ActionEvent e = new ActionEvent(this, 1, "Event Triggered");
		for(int i=0;i<actionListeners.size();i++)
			actionListeners.get(i).actionPerformed(e);
	}
	
	public int compareTo(Object o)
	{
		AlarmEvent e = (AlarmEvent) o;
		return this.date.compareTo(e.date);
	}
}
