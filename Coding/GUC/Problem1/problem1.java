import java.io.*;
public class Problem1 {
	public static void main(String args[]) throws IOException {
	
		InputStreamReader inStream=new InputStreamReader(System.in);
		BufferedReader stdin=new BufferedReader(inStream);
		System.out.println("Please enter the number you wish to display:");
		String n=stdin.readLine();
		System.out.println("Please enter the desired size of the displayed digits:");
		int s = Integer.parseInt(stdin.readLine());
		
		int _numlen=1;
		int _rows=1;
		int _columns=n.length()*(s+2);
		boolean done=false;
		int k[]=new int[n.length()];
		for(int x=0; x<n.length(); x++){
			k[x]=Integer.parseInt(Character.toString(n.charAt(x)));
		}
			
			//Loop 1
			for(int i=0; i<n.length(); i++){
				System.out.print(" ");
				if(k[i]==0||k[i]==2||k[i]==3||k[i]>=5){
					for(int j=0; j<s;j++){
						System.out.print("-");
					}
					System.out.print(" ");
				}	
				else if(k[i]==1||k[i]==4){
					for(int j=0; j<s;j++){
						System.out.print(" ");
					}
					System.out.print(" ");	
				}
				System.out.print("  ");
				}
				System.out.print("\n");
			
			//Loop 2
			for(int h = 0; h<s; h++){
				for(int i=0; i<n.length(); i++){
					if(k[i]==0||k[i]==4||k[i]==8||k[i]==9){
						System.out.print("|");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print("|");
					}
					else if(k[i]==1||k[i]==2||k[i]==3||k[i]==7){
						System.out.print(" ");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print("|");
					}
					else if(k[i]==5||k[i]==6){
						System.out.print("|");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print(" ");
					}
					System.out.print("  ");	
				}
				System.out.print("\n");
			}
				
			//Loop 3
			for(int i=0; i<n.length(); i++){
				if((k[i]>=2&&k[i]<=6)||k[i]>=8){
					System.out.print(" ");
					for(int j=0; j<s;j++){
						System.out.print("-");
					}
					System.out.print(" ");
				}	
				else if(k[i]==0||k[i]==1||k[i]==7||k[i]==9){
					System.out.print(" ");
					for(int j=0; j<s;j++){
						System.out.print(" ");
					}
					System.out.print(" ");	
				}
				System.out.print("  ");
				}
				System.out.print("\n");

			//Loop 4
			for(int f = 0; f<s; f++){
				for(int i=0; i<n.length(); i++){
					if(k[i]==0||k[i]==6||k[i]==8){
						System.out.print("|");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print("|");
					}
					else if(k[i]==1||k[i]==3||k[i]==4||k[i]==5||k[i]==7||k[i]==9){
						System.out.print(" ");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print("|");
					}
					else if(k[i]==2){
						System.out.print("|");
						for(int j=0; j<s;j++){
							System.out.print(" ");
						}
						System.out.print(" ");
					}
					System.out.print("  ");	
				}
				System.out.print("\n");
				}	
			
			//Loop 5
			for(int i=0; i<n.length(); i++){
				if(k[i]==0||k[i]==2||k[i]==3||k[i]==5||k[i]==6||k[i]==8||k[i]==9){
					System.out.print(" ");
					for(int j=0; j<s;j++){
						System.out.print("-");
					}
					System.out.print(" ");
				}
				else if(k[i]==1||k[i]==4||k[i]==7){
					System.out.print(" ");
					for(int j=0; j<s;j++){
						System.out.print(" ");
					}
					System.out.print(" ");	
				}
				System.out.print("  ");
				}
			System.out.print("\n");			
	}
}
