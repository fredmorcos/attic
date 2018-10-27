/*
	Ahmad Hisham
	Frederic-Gerald Morcos
	
	This is where the function that parses
		the incoming data is, used on both
		the server and the client.
		
	TODO:
		* We could use a string tokenizer for
			1) increased performance
			2) less code :P thus less bugs :P
 */

package engine;
/* SEE:	docs/protocol
 */

public class protocol {

	/* will parse a message string and return
	 * it in an array for easier reading in the
	 * server/client.
	 * 
	 * protocol format:
	 * 		COM,arg1,arg2
	 * 
	 * structure of array:
	 * 		string[0]: command
	 * 		string[1]: nickname
	 * 		string[2]: data
	 * 
	 * return string[]: parsed message
	 * return null: error parsing
	 */
	public static String [] parse_message(String message) {
		/* check that the command is of at least
		 * length 5 (3 for command + 2 for ",,") 
		 */
		try {
			if(message.length() >= 5 && message.contains(",")) {
				if(message.substring(0, message.indexOf(",")).length() != 3) {
					return null;
				}
				else {
					String temp_nickname = "";
					String temp_command = "";
					String temp_data = "";
					if(message.indexOf(",") != -1) {
						temp_command = message.substring(0, message.indexOf(","));
						message = message.substring(message.indexOf(",")+1);
					}
						
					if(message.indexOf(",") != -1) {
						temp_nickname = message.substring(0, message.indexOf(","));
					}
							
					if(message.indexOf(",") != -1) {
						temp_data = message.substring(message.indexOf(",")+1);
					}
					return new String[] {temp_command, temp_nickname, temp_data};
				}
			}
			else {
				return null;
			}
		}
		catch(Exception e) {
			System.out.println("[error] protocol_parse_message - exception: " + e);
			return null;
		}
	}
	
	public static void main(String args[]) {
		protocol test = new protocol();
		String x[] = test.parse_message("COM,NICKNAME,BLABLA,BLA,BLA");
		/* String x[] = test.parse_message("COM,,"); */
		if(x != null) {
			for(int i=0; i<x.length; i++) {
				System.out.println(x[i]);
			}
		}
		else {
			System.out.println("Error parsing...");
		}
	}
}
