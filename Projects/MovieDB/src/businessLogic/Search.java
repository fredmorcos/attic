package businessLogic;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.LinkedList;

import javax.xml.xpath.*;

import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
public class Search {
	static LinkedList titleList;
	static LinkedList yearList;
	static LinkedList directorList;
	static LinkedList actorList;
	static LinkedList actressList;
	static LinkedList companyList;
	static LinkedList languageList;
	static LinkedList countryList;
	public static LinkedList getTitleList()
	{
		return titleList;
	}
	public static LinkedList getYearList()
	{
		return yearList;
	}
	public static LinkedList getLanguageList()
	{
		return languageList;
	}
	public static LinkedList getCountryList()
	{
		return countryList;
	}
	public static LinkedList getActorList()
	{
		return actorList;
	}
	public static LinkedList getActressList()
	{
		return actressList;
	}
	public static LinkedList getDirectorList()
	{
		return directorList;
	}
	public static boolean doTheSearch(String exp)
	{
		titleList=new LinkedList();
		yearList=new LinkedList();
		directorList=new LinkedList();
		actorList=new LinkedList();
		actressList=new LinkedList();
		companyList=new LinkedList();
		languageList=new LinkedList();
		countryList=new LinkedList();
		XPathFactory  factory=XPathFactory.newInstance();
		XPath xPath=factory.newXPath();
		File xmlDocument = new File("movie_database_for_schema.xml");
		
		try {
			
			InputSource inputSource = new InputSource(new FileInputStream(xmlDocument));
			try {
				String expression = exp + "/title";
				XPathExpression  xPathExpression= xPath.compile(expression);
				NodeList titles = (NodeList) xPathExpression.evaluate(inputSource, XPathConstants.NODESET);
				
				if(titles.getLength()==0)
				{
					System.out.println("EMPTYYY LISTTT");
					return false;
				}
				for(int i=0;i<titles.getLength();i++)
					{
						if (titles.item(i)!=null)
						titleList.add((String)(titles.item(i).getTextContent()));
					}
				InputSource inputSource1 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp + "/year";
				XPathExpression xPathExpression1= xPath.compile(expression);
				NodeList years = (NodeList) xPathExpression1.evaluate(inputSource1, XPathConstants.NODESET);
				System.out.println("el 3'alta henaaa");
				for(int i=0;i<years.getLength();i++)
					{
						if(years.item(i)!=null)
						yearList.add((String)(years.item(i).getTextContent()));
					}
				InputSource inputSource2 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp +"/language";
				XPathExpression xPathExpression2= xPath.compile(expression);
				NodeList languages = (NodeList) xPathExpression2.evaluate(inputSource2, XPathConstants.NODESET);
				for(int i=0;i<languages.getLength();i++)
					{
						if(languages.item(i)!=null)
						languageList.add((String)(languages.item(i).getTextContent()));
					}
				InputSource inputSource3 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp +"/country";
				XPathExpression xPathExpression3= xPath.compile(expression);
				NodeList countries = (NodeList) xPathExpression3.evaluate(inputSource3, XPathConstants.NODESET);
				for(int i=0;i<countries.getLength();i++)
					{
						if(countries.item(i)!=null)
						countryList.add((String)(countries.item(i).getTextContent()));
					}
				InputSource inputSource4 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp+"/major_actors/actor";
				XPathExpression xPathExpression4= xPath.compile(expression);
				NodeList actors = (NodeList) xPathExpression4.evaluate(inputSource4, XPathConstants.NODESET);
				for(int i=0;i<actors.getLength();i++)
					{
						if(actors.item(i)!=null)
						actorList.add((String)(actors.item(i).getTextContent()));
					}
				InputSource inputSource5 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp+"/major_actors/actriss";
				XPathExpression xPathExpression5= xPath.compile(expression);
				NodeList actresses = (NodeList) xPathExpression5.evaluate(inputSource5, XPathConstants.NODESET);
				for(int i=0;i<actresses.getLength();i++)
					{
						if(actresses.item(i)!=null)
						actressList.add((String)(actresses.item(i).getTextContent()));
					}
				InputSource inputSource6 = new InputSource(new FileInputStream(xmlDocument));
				expression = exp+"/director";
				XPathExpression xPathExpression6= xPath.compile(expression);
				NodeList directors = (NodeList) xPathExpression6.evaluate(inputSource6, XPathConstants.NODESET);
				for(int i=0;i<directors.getLength();i++)
					{
						if(directors.item(i)!=null)
						directorList.add((String)(directors.item(i).getTextContent()));
					}
			
			} catch (XPathExpressionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}
	public static boolean searchByTitle(String title)
	{
		String expression = "//movie[title='" + title + "']";
		return doTheSearch(expression);
	}
	public static boolean searchByCountry(String country)
	{
		String expression = "//movie[country='" + country + "']";
		return doTheSearch(expression);
	}
	public static boolean searchByLanguage(String language)
	{
		String expression = "//movie[language='" + language + "']";
		return doTheSearch(expression);
	}
	public static boolean searchByActor(String actor)
	{
		String expression = "//movie[major_actors/actor='" + actor + "']";
		return doTheSearch(expression);
	}
	public static boolean searchByActress(String actress)
	{
		String expression = "//movie[major_actors/actriss='" + actress + "']";
		return doTheSearch(expression);
	}
}
