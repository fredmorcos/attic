//package engine;

import SQLite.*;

public class engine implements Callback, Function, Authorizer, Trace, ProgressHandler {
	private StringBuffer acc = new StringBuffer();
	
	String		filename;
	int		number_of_table_rows;
	int		number_of_table_columns;
	Database	db;
	
	public engine (String fn) {
		filename = fn;
		db = new Database();
		try {
			System.out.println("[engine] opening databse...");
			db.open(filename, 0666);
			System.out.println("[engine] database opened.");
			
			/* String test[][] = table_to_array(get_table("user_account", new String[] {"firstname", "lastname"}, 
						"x.lastname='basta'").toString());
			System.out.println(test[0][0] + " -- " + test[0][1]);
			System.out.println(test[1][0] + " -- " + test[1][1]); */
			
			System.out.println(do_login("nardinebasta", "narnarnar", "student"));
			
			db.close();
			System.out.println("[engine] database closed.");
		}
		catch (SQLite.Exception e) {
			System.out.println("[engine] [exception] " + e);
		}
	}
	
	public boolean do_login(String username, String password, String type) {
		try {
			String x[][] = table_to_array(get_table_from_query("SELECT COUNT(id) FROM user_account x " +
												"WHERE x.username='" + username + "' AND " +
												"x.password='" + password + "' AND " +
												"x.type='" + type + "'").toString());
			if(Integer.parseInt(x[1][0]) > 0) {
				if(Integer.parseInt(x[1][0]) == 1) {
					return true;
				}
				else {
					System.out.println("[engine - do_login] [message] more than one account to login with.");
					return false;
				}
			}
			else {
				return false;
			}
		}
		catch(java.lang.Exception e) {
			System.out.println("[engine - do_login] [exception] " + e);
			return false;
		}
	}
	
	/* the table is returned in a string [rows][columns] */
	public String[][] table_to_array(String table) {
		String lines[] = table.split("\n");
		//String temp[] = lines[0].split("|");
		int count=0;
		for(int a=0;a<lines[0].length();a++){
			if(lines[0].charAt(a)=='|')
				count++;
		}
		String data[][] = new String[lines.length][count];
		for(int i=0; i<lines.length; i++) {
			//temp = lines[i].split("|");
			int j=0;
			while( j<count) {
				//data[i][j] = temp[j];
				String s="";
				for(int x=0;x<lines[i].length();x++){
					if(lines[i].charAt(x)!='|')
						s+=lines[i].charAt(x);
					else {
						data[i][j]=s;
						j++;
						s="";
					}	
				}
			}
		}
		return data;
	}
	
	public boolean execute(String query) {
		try {
			db.exec(query, null);
			return true;
		}
		catch (SQLite.Exception e) {
			System.out.println("[engine - execute] [exception] " + e);
			return false;
		}
	}
	
	public String get_table_from_query(String query) {
		try {
			return db.get_table(query).toString();
		}
		catch(SQLite.Exception e) {
			System.out.println("[engine - get_table] [exception] " + e);
			return null;
		}
	}
	
	public String get_table(String tablename, String columnnames[]) {
		String columns = "";
		for(int i=0; i<columnnames.length; i++) {
			columns = columns + "x." + columnnames[i] + ",";
		}
		columns = columns.substring(0, columns.length()-1);
		try {
			return db.get_table("SELECT " + columns + 
								" FROM " + tablename + " x").toString();
		}
		catch(SQLite.Exception e) {
			System.out.println("[engine - get_table] [exception] " + e);
			return null;
		}
	}
	
	public String get_table(String tablename, String columnnames[], String condition) {
		String columns = "";
		for(int i=0; i<columnnames.length; i++) {
			columns = columns + "x." + columnnames[i] + ",";
		}
		columns = columns.substring(0, columns.length()-1);
		try {
			return db.get_table("SELECT " + columns + 
								" FROM " + tablename + " x" + 
								" WHERE " + condition).toString();
		}
		catch(SQLite.Exception e) {
			System.out.println("[engine - get_table] [exception] " + e);
			return null;
		}
	}
	
	public String get_table(String tablename) {
		try {
			return db.get_table("SELECT * " +
								"FROM " + tablename).toString();
		}
		catch(SQLite.Exception e) {
			System.out.println("[engine - get_table] [exception] " + e);
			return null;
		}
	}
	
	public String get_table(String tablename, String condition) {
		try {
			return db.get_table("SELECT * " +
								"FROM " + tablename + " x " +
								"WHERE " + condition).toString();
		}
		catch(SQLite.Exception e) {
			System.out.println("[engine - get_table] [exception] " + e);
			return null;
		}
	}
	
    public void columns(String col[]) {
    	System.out.println("#cols = " + col.length);
    	for (int i = 0; i < col.length; i++) {
    	    System.out.println("col" + i + ": " + col[i]);
    	}
    	// throw new java.lang.RuntimeException("boom");
        }

    public void types(String types[]) {
    	if (types != null) {
    	    for (int i = 0; i < types.length; i++) {
    	        System.out.println("coltype" + i + ": " + types[i]);
    	    }
    	}
    }

    public boolean newrow(String data[]) {
    	for (int i = 0; i < data.length; i++) {
    	    System.out.println("data" + i + ": " + data[i]);
    	}
    	return false;
    }

    public void function(FunctionContext fc, String args[]) {
    	System.out.println("function:");
    	for (int i = 0; i < args.length; i++) {
    	    System.out.println("arg[" + i + "]=" + args[i]);
    	}
    	if (args.length > 0) {
    	    fc.set_result(args[0].toLowerCase());
    	}
    }

    public void step(FunctionContext fc, String args[]) {
    	System.out.println("step:");
    	for (int i = 0; i < args.length; i++) {
    	    acc.append(args[i]);
    	    acc.append(" ");
    	}
    }

    public void last_step(FunctionContext fc) {
    	System.out.println("last_step");
    	fc.set_result(acc.toString());
    	acc.setLength(0);
    }

    public int authorize(int what, String arg1, String arg2, String arg3,
    			 String arg4) {
    	System.out.println("AUTH: " + what + "," + arg1 + "," + arg2 + ","
    			   + arg3 + "," + arg4);
    	return Constants.SQLITE_OK;
    }

    public void trace(String stmt) {
    	System.out.println("TRACE: " + stmt);
    }

    public boolean progress() {
    	System.out.println("PROGRESS");
    	return true;
    }
	
	public static void main(String args[]) {
		engine test = new engine("../sql/elearning.db");
	}
}