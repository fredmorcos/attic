package businessLogic;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedList;

import javax.xml.xpath.*;

import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

public class LoginChecker {
	
	public static boolean checkLogin(String user,String password)
	{
		
		File file = new File("test.txt");
		System.out.println(file.getAbsolutePath());
		
		System.out.println("LoginChecker checking login " + user + ", " + password);
		XPathFactory  factory=XPathFactory.newInstance();
		XPath xPath=factory.newXPath();
		File xmlDocument = new File("movie_database_for_schema.xml");
		try {
			InputSource inputSource = new InputSource(new FileInputStream(xmlDocument));
			
			try {
				String expression = "//user[@username='" + user + "']/@password";
				System.out.println("LoginChecker evaluating expession: " + expression);
				XPathExpression  xPathExpression= xPath.compile(expression);
				
		//		NodeList result = (NodeList) xPathExpression.evaluate(inputSource, XPathConstants.NODESET);
				String result = (String) xPathExpression.evaluate(inputSource, XPathConstants.STRING);
			
				System.out.println("LoginChecker result " + result);
				if(user==null || password==null)
				{
					return false;
				}
				else
				{
					if (result.compareTo(password)==0)
				{
					return true;
				}
					else
					{
						System.out.println("you didn't enter a valid password");
						System.out.println(result);
						return false;
					}
				
				}
			
		
			
			} catch (XPathExpressionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return false;
		
	}
	
	public static void main(String[] args) 
	{
		System.out.println(LoginChecker.checkLogin("christine", "christy"));
		
		
	}

}
