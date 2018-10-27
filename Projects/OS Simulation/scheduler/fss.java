import java.util.LinkedList;
public class fss
{
	public LinkedList<process> jobs;
	static int clock;
	boolean flag;// for  managing the clock tics
	boolean flagindex;
	int Index;
	LinkedList current_owners;
	LinkedList already_ran;
	boolean flag2;
	
	public fss()
	{
		jobs = new LinkedList<process>();
		already_ran = new LinkedList();
		current_owners = new LinkedList();
		clock=0;
		flag=false;
		flag2=false;
		flagindex=false;
		int Index=0;
	}
	
	/*
	 *this method is for initializing a linked list with the names of the users avalable
	 */
	
	public  LinkedList get_current_owners()
	{
		LinkedList current_owners = new LinkedList();
			
		if(jobs.size()==0) 
		return null;
		
		else
		{
			int index=0;
			
			while(index<jobs.size())
			{
				if(!current_owners.contains(((process)jobs.get(index)).get_owner_name()))
				{
					current_owners.add(((process)jobs.get(index)).get_owner_name());
				}
				index++;
			}	
			
			}
			
		
		
		return current_owners;
	}
	
	/*
	 *this method is for initializing a linked list that  goes in parallell with the
	 *linked list of the processes to make sure that each user's turn a differnt one ofhis
	 *processes runs. beeing false emans that that process didn't run b4
	 */
	public  LinkedList already_ran()
	{
		if (jobs.size()==0)
		{ 
			return null;
		}
		else
		{
		
			for(int i=0;i<jobs.size();i++)
			{
				already_ran.add("false");
			}
		
			return already_ran;
		}
	}
	
	/*
	 *makes sure that the process who has the turn to run hasn't ran b4
	 *which means that all the user's processes has ran b4 
	 *ie something like round robin
	 *
	 */		
	public int check_processes()
	{

		String name =((process)jobs.get(Index)).get_owner_name();
		boolean fl = false;
		boolean fl2 = false;
		int i;
		
				
		for(int j=0;j< jobs.size()-Index;j++)
		{
			if(((process)jobs.get(j)).get_owner_name()==name&&already_ran.get(j)=="false")
			{
				fl2=true;
				break;
			}
		}
		if(fl2==false)
		{
			for(int j=0; j<jobs.size(); j++)
			{
				if(((process)jobs.get(j)).get_owner_name()==name)
				{
					already_ran.add(j,"false");
				}
			}
		}
		
			for(i=Index; i<jobs.size(); i++) {
				if(((process)jobs.get(i)).get_owner_name()==name && 
					already_ran.get(i)=="false" && 
					((process)jobs.get(i)).get_arrive_time()<=clock && 
					((process)jobs.get(i)).get_run_time()>0)
				{
					fl=true;
					already_ran.add(i,"true");
					break;
				}
			}

			return i;
	}
	/*
	 *ths methoid is for running the processes
	 *the priority here is handelled as follows
	 *if a user has a priority x the process having the turn will run for x quantums
	 *
	 */
	public void run(int index) {
		for(int i=0;i<((process)jobs.get(index)).get_owner_priority();i++) {
			int run_time=((process)jobs.get(index)).get_run_time()-1;
			
			System.out.println("name "+((process)jobs.get(index)).get_name());		
			System.out.println("owner "+((process)jobs.get(index)).get_owner_name());
			System.out.println("process "+index+"is running");
			System.out.println("priority "+((process)jobs.get(index)).get_owner_priority());
			System.out.println("runtime "+((process)jobs.get(index)).get_run_time()+"is running"+'\n');
					
			((process)jobs.get(index)).set_run_time(run_time);
			current_owners.remove(((process)jobs.get(index)).get_owner_name());
			
			if(((process)jobs.get(index)).get_run_time()==0) {
				clock++;
				System.out.println("clock=  "+clock);
				flag=true;
				flagindex=true;
				break;
			}
			clock++;
			flag=true;
			System.out.println("clock=  "+clock);
		}
	}
	
	/*
	 *this method do the schedueling
	 *it moves the index around the linked list of the processes and assign the
	 * turn to the process and each time increments the clock
	 */
	 
	public void scheduel() {
		LinkedList already_ran =already_ran();
		while(jobs.size()!=0) {
			Index=0;
			while(Index<jobs.size()) {
				flag =false;
				flagindex=false;
				
				if(current_owners.size()==0) {
					current_owners =get_current_owners();
				}
				
				if(((process)jobs.get(Index)).get_arrive_time()<=clock) {
					if(current_owners.contains(((process)jobs.get(Index)).get_owner_name())) {
						int i = check_processes();
						run(i);		
			    		}
				}
				else {
					System.out.println("no current jobs available");
					if(Index==jobs.size()-1) {
						if(!flag) {
						
							clock++;
							flag=true;
						}
						flag2=false;
						System.out.println("clock=  "+clock);
					}
				}
				//if the process has a runtime= 0 it is removed from the list 
				if(((process)jobs.get(Index)).get_run_time()==0) {
					jobs.remove(Index);
					already_ran.remove(Index);
				}
				
				if(flagindex==false) {
					Index++;
				}
			
				if(Index<jobs.size()-1&&((process)jobs.get(Index)).get_arrive_time()>clock) {
					Index=0;
					current_owners =get_current_owners();
					if(!flag) {
						clock++;
						flag=true;
					}
					flag=false;
					flag2=false;
					System.out.println("clock=  "+clock);
				}	
			}	
		}	
	}
}
