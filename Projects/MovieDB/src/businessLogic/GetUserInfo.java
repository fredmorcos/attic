package businessLogic;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedList;

import javax.xml.xpath.*;

import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

public class GetUserInfo {
	static String password;
	static String address;
	static String email;
	static String fname;
	static String lname;
	public static String getPassword()
	{
		return password;
	}
	public static String getAddress()
	{
		return address;
	}
	public static String getEmail()
	{
		return email;
	}
	public static String getFname()
	{
		return fname;
	}
	public static String getLname()
	{
		return lname;
	}
	public static void getInfo(String username)
	{
		password="";
		address="";
		email="";
		fname="";
		lname="";
		XPathFactory  factory=XPathFactory.newInstance();
		XPath xPath=factory.newXPath();
		File xmlDocument = new File("movie_database_for_schema.xml");
		try {
			InputSource inputSource1 = new InputSource(new FileInputStream(xmlDocument));
			
			try {
				String expression1 = "//user[@username='" + username + "']/@password";
				XPathExpression  xPathExpression1= xPath.compile(expression1);
				password = (String) xPathExpression1.evaluate(inputSource1, XPathConstants.STRING);
				
				InputSource inputSource2 = new InputSource(new FileInputStream(xmlDocument));
				String expression2 = "//user[@username='" + username + "']/adress";
				XPathExpression  xPathExpression2= xPath.compile(expression2);
				address = (String) xPathExpression2.evaluate(inputSource2, XPathConstants.STRING);
	
				InputSource inputSource3 = new InputSource(new FileInputStream(xmlDocument));
				String expression3 = "//user[@username='" + username + "']/email";
				XPathExpression  xPathExpression3= xPath.compile(expression3);
				email = (String) xPathExpression3.evaluate(inputSource3, XPathConstants.STRING);
			
				InputSource inputSource4 = new InputSource(new FileInputStream(xmlDocument));
				String expression4 = "//user[@username='" + username + "']/name/f_name";
				XPathExpression  xPathExpression4= xPath.compile(expression4);
				fname = (String) xPathExpression4.evaluate(inputSource4, XPathConstants.STRING);
				
				InputSource inputSource5 = new InputSource(new FileInputStream(xmlDocument));
				String expression5 = "//user[@username='" + username + "']/name/l_name";
				XPathExpression  xPathExpression5= xPath.compile(expression5);
				lname = (String) xPathExpression5.evaluate(inputSource5, XPathConstants.STRING);
			
			} catch (XPathExpressionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
