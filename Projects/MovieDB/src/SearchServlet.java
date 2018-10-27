import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import businessLogic.*;


public class SearchServlet extends HttpServlet {
	
	public static boolean checkSearchType(String toDo,String title,String country,String actor,String actress,String language)
	{
		if(toDo.compareTo("title")==0)
		{
			return Search.searchByTitle(title);
		}
		else if(toDo.compareTo("country")==0)
		{
			System.out.println("Search by countryyy");
			return Search.searchByCountry(country);
		}
		else if(toDo.compareTo("language")==0)
		{
			System.out.println("Search by languageee");
			return Search.searchByLanguage(language);
		}
		else if(toDo.compareTo("actor")==0)
		{
			return Search.searchByActor(actor);
		}
		else if(toDo.compareTo("actress")==0)
		{
			return Search.searchByActress(actress);
		}
		else return false;
			
	}
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String toDo = (String)(request.getParameter("toDo"));
		String title = (String)(request.getParameter("movieName"));
		String country = (String)(request.getParameter("countryName"));
		String actor = (String)(request.getParameter("actorName"));
		String actress = (String)(request.getParameter("actressName"));
		String language = (String)(request.getParameter("languageName"));
		if(checkSearchType(toDo,title,country,actor,actress,language))
		{
			List titles=(List)(Search.getTitleList());
			request.setAttribute("titleList",titles );
			List years=(List)(Search.getYearList());
			request.setAttribute("yearList", years);
			List languages=(List)(Search.getLanguageList());
			request.setAttribute("languageList",languages);
			List countries=(List)(Search.getCountryList());
			request.setAttribute("countryList",countries);
			List actors=(List)(Search.getActorList());
			request.setAttribute("actorList",actors);
			List actresses=(List)(Search.getActressList());
			request.setAttribute("actressList",actresses);
			List directors=(List)(Search.getDirectorList());
			request.setAttribute("directorList",directors);
		}
		String url = "mainPage.jsp";
		RequestDispatcher dispatcher;
		dispatcher = request.getRequestDispatcher(url);
		dispatcher.forward(request, response);
	}

	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// doPost shall do the same as doGet
		// in this case it doesn't matter
		// where the code is implemented (in doPost or doGet)
		doGet(request, response);
	}
}
