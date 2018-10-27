package businessLogic;

import java.io.*;
import java.util.*;

import javax.xml.*;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.NodeList;

public class xmlReader {
	XPathFactory factory;
	File document;
	XPath expression;
	
	public xmlReader (String fn) {
		factory = XPathFactory.newInstance();
		document = new File(fn);
		expression = factory.newXPath();
	}
	
	public boolean exists (String expression) {
		return false;
	}
	
	public static void main (String args[]) {
		xmlReader test = new xmlReader("test.xml");
		System.out.println (test.exists ("//movie_db/user[@username=christine]"));
	}
}