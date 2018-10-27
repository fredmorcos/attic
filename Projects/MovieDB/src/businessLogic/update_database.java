package businessLogic;

import java.util.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import javax.xml.bind.*;
import javax.xml.xpath.*;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import org.xml.sax.InputSource;
//import javax.xml.bind.helper;
//import javax.xml.bind.util;
import java.io.*;
import db_package.*;

public class update_database
{
	public update_database() {}
	
	public static boolean update(String username, String password, String email,
	                       String adress,String Fname, String Lname)
	{ 
		if (checkUnique(username))
		{
		try
		{
		 	JAXBContext jc=JAXBContext.newInstance("db_package");

   			 System.out.println("context ok");
   			 
   			 Unmarshaller unmarshaller = jc.createUnmarshaller();
   			 JAXBElement<MovieDb> movie_db= (JAXBElement) 
   			 unmarshaller.unmarshal(new File( "movie_database_for_schema.xml"));
   			 
   			 MovieDb mdb = movie_db.getValue();

   			 System.out.println("got xml doc");
   			 
   			 List<UserType> user=mdb.getUser();
  
   			 String use =user.get(0).getUsername();
   			 System.out.println(use);
   			 
   			 ObjectFactory objFactory = new ObjectFactory();
   			 UserType new_user = objFactory.createUserType();
   			 new_user.setUsername(username);
   			 new_user.setPassword(password);
   			 new_user.setEmail(email);
   			 new_user.setAdress(adress);
   			 ObjectFactory objFactory_2 = new ObjectFactory();
   			 NameType new_name =objFactory_2.createNameType();
   			 new_name.setFName(Fname);
   			 new_name.setLName(Lname);
   			 new_user.setName(new_name);
   			 
   			 user.add(new_user);
   			 
   			 username =user.get(1).getUsername();
   			 System.out.println(username);
   			 
   			 Marshaller marshaller = jc.createMarshaller();
   			 System.out.println("marshaller  ready");
   			 
   			 
   			 marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

			 marshaller.marshal(movie_db, new FileOutputStream("movie_database_for_schema.xml"));

 
    		 System.out.println("java tree converted into xml & filed");

    		 return true;

   			 
		 }
		 
		 catch(Exception e1)

   		 {
   		 	System.out.println("lala  "+e1);  
   		 	return false;
   		 }
		}
		else
		{
			System.out.println("user already registered");
			return false;
		}

		
	} 
	public static boolean checkUnique(String username)
	{
		XPathFactory  factory=XPathFactory.newInstance();
		XPath xPath=factory.newXPath();
		File xmlDocument = new File("movie_database_for_schema.xml");
		try {
			InputSource inputSource = new InputSource(new FileInputStream(xmlDocument));
			String expression = "//user/@username";
			System.out.println("LoginChecker evaluating expession: " + expression);
			try 
			{
				XPathExpression  xPathExpression= xPath.compile(expression);
				NodeList result = (NodeList) xPathExpression.evaluate(inputSource, XPathConstants.NODESET);
				for(int i=0;i<result.getLength();i++)
				{
					if(((String)result.item(i).getNodeValue()).compareTo(username)==0) {
						{
							System.out.println("user already REGISTERED");
							return false;
						}
					}
				}
			} 
			catch (XPathExpressionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
	//		NodeList result = (NodeList) xPathExpression.evaluate(inputSource, XPathConstants.NODESET);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}
	
}