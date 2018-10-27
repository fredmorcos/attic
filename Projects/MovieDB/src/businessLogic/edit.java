package businessLogic;

import java.io.File;
import java.io.FileOutputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import db_package.*;


public class edit {
	
	
	public static void update_account(String username, String password, String email,
            String address,String fname, String lname)
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
  			 int i;
  			 for(i=0;i<user.size();i++)
  			 {
 
  				 String use =user.get(i).getUsername();
  				 System.out.println(use);
  				 if(use.compareTo(username)==0)
  					 break;
  			 }
  			 
  			if (password!=null && password!="")
  			user.get(i).setPassword(password);
  			if(address!=null && address!="")
  			user.get(i).setAdress(address);
  			if(email!=null && email!="")
  			user.get(i).setEmail(email);
  			if(fname!=null && fname!="")
  			user.get(i).getName().setFName(fname);
  			if(lname!=null && lname!="")
  			user.get(i).getName().setLName(lname);
  			/*
  			 ObjectFactory objFactory = new ObjectFactory();
  			 NameType new_name =objFactory.createNameType();
  			 if(fname!=null)
  			 new_name.setFName(fname);
  			 if(lname!=null)
  			 new_name.setLName(lname);
  			 user.get(i).setName(new_name); 
  			 */
  			 Marshaller marshaller = jc.createMarshaller();
   			 System.out.println("marshaller  ready");
   			 
   			 
   			 
   			 marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

			 marshaller.marshal(movie_db, new FileOutputStream("movie_database_for_schema.xml"));

 
    		 System.out.println("java tree converted into xml & filed");

  			

		}
		catch(Exception e)
		{
			System.out.println(e);	
		}
		}
	}


