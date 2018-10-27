/* main class
 * gets the arguments, sets the processes and users,
 * and starts the respective scheduling algorithm.
 */

import java.util.LinkedList;

public class main {
	public static void main(String args[]) {
		System.out.println("Initializing users and processes...");

		/* for the fair-share scheduling */
		user a = new user("a",1);
		user b = new user("b",2);
		user c = new user("c",3);
		user d = new user("d",4);
		user e = new user("e",5);
		
		process pa= new process("a",1,3,a);
		process pb= new process("b",2,4,b);
		process pc= new process("c",9,6,c);
		process f= new process("c-",10,5,c);
		process pd= new process("d",11,6,d);
		process pe= new process("e",12,7,e);
		/*************************************/
	
		/* for the multilevel feedback scheduling */
		user u1 = new user("user1", 0);
		
		process p1 = new process("process1", 0, 3, u1);
		process p2 = new process("process2", 1, 5, u1);
		process p3 = new process("process3", 3, 2, u1);
		process p4 = new process("process4", 7, 20, u1);
		process p5 = new process("process5", 10, 5, u1);
		
		LinkedList<process> list = new LinkedList<process>();
		list.add(p1);
		list.add(p2);
		list.add(p3);
		list.add(p4);
		list.add(p5);
		/****************************************************/
		
		/* for the lottery scheduling */
		user user1 = new user("user1", 3);
		process proc1 = new process("process1", 7, 2, user1);
		process proc2 = new process("process2", 0, 4, user1);
		
		user user2 = new user("user2", 2);
		process proc3 = new process("process3", 0, 4, user2);
		process proc4 = new process("process4", 1, 2, user2);
		
		user user3 = new user("user3", 5);
		process proc5 = new process("process5", 6, 6, user3);
		
		LinkedList<process> plist = new LinkedList<process>();
		plist.add(proc1);
		plist.add(proc2);
		plist.add(proc3);
		plist.add(proc4);
		plist.add(proc5);
		/*****************************************************/
		
		if (args.length < 1) {
			System.out.println("Too few arguments.\n" + 
						"Usage: java main <algorithm_name>\n\n" + 
						"\tName\tDescription\n" + 
						"\t----\t-----------\n" + 
						"\tmlf \tMulti-level Feedback Scheduling\n" + 
						"\tlot \tLottery Scheduling\n" + 
						"\tfss \tFair-Share Scheduling\n");
		}
		else if (args[0].compareTo("mlf") == 0) {
			System.out.println("Selected the Multi-Level Scheduling Algorithm.");
			/* run the mlf class */
			mlf sched = new mlf(list);
		}
		else if (args[0].compareTo("lot") == 0) {
			System.out.println("Selected the Lottery Scheduling Algorithm.");
			/* run the lot class */
			lot sched = new lot(plist);
		}
		else if (args[0].compareTo("fss") == 0) {
			System.out.println("Selected the Fair-Share Scheduling Algorithm.");
			/* run the fss class */
			fss fs = new fss();
			fs.jobs.add(pa);
			fs.jobs.add(pb);
			fs.jobs.add(pc);
			fs.jobs.add(f);
			fs.jobs.add(pd);
			fs.jobs.add(pe);
			fs.scheduel();
			fs.scheduel();
		}
		else {
			System.out.println("Unknown scheduling algorithm selected.\n" +
						"Run 'java main' without any arguments to\n" +
						"get the list of supported algorithms.");
		}
	}
}
