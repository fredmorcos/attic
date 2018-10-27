/*
 * Alarm.java
 *
 * Created on October 28, 2007, 11:45 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

/**
 *
 * @author Drew
 */

import java.util.*;
public class Alarm extends Thread
{
	Vector<AlarmEvent> events;
	/** Creates a new instance of Alarm */
	public Alarm() 
	{
		events = new Vector<AlarmEvent>();
		
	}
	
	public void run()
	{
		while(true)
		{
			System.out.println("Checking ...");
			while(!events.isEmpty()&&events.firstElement().getDate().compareTo(Calendar.getInstance())<=0)
			{
				System.out.println("TRRRN : "+events.firstElement().getMessage());
				events.firstElement().trigger();
				events.remove(0);
			}
			try
			{
				this.sleep(1000);
			}
			catch(Exception e)
			{
				System.out.println(e);
			}
		}
	}
	
	public void addEvent(AlarmEvent e)
	{
		events.add(e);
		Collections.sort(events);
	}
	
	public void removeEvent(AlarmEvent e)
	{
		events.remove(e);
	}
	
	public Vector getEvents()
	{
		return (Vector)events.clone();
	}
	
	
}
