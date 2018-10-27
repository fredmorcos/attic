import java.io.IOException;
//import java.util.LinkedList;
//import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import businessLogic.update_database;


public class RegisterServlet extends HttpServlet {
	public void doGet(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {
		
		/*String toDo = request.getParameter("todo");
		
		if (toDo == null) {
			toDo = "";
		}
		else if (toDo.compareTo("showText") == 0) {
			// read parameters set by JSP
			String username = request.getParameter("textUsername");
			String password = request.getParameter("textPassword");
			String address = request.getParameter("textAdress");
			String email = request.getParameter("textEmail");
			
			// data to be passed to the JSP page
			request.setAttribute("txt1", username);
			request.setAttribute("txt2", password);

			// define the action to be taken by the JSP
			request.setAttribute("action", "showText");
		}
		*/
		String toDo=(String)request.getParameter("todo");
		if(toDo!=null){
		
		String username =(String)( request.getParameter("textUsername"));
		String password =(String) (request.getParameter("textPassword"));
		String address =(String) (request.getParameter("textAddress"));
		String email =(String) (request.getParameter("textEmail"));
		String fname =(String) (request.getParameter("fname"));
		String lname =(String) (request.getParameter("lname"));
		if(update_database.update(username,password,email,address,fname,lname))
		{
			String url = "mainPage.jsp";
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
		}
		else
		{
			String url = "index.html";
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
		}
		
		}
		else 
		{
			String url = "registerPage.jsp";
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
		}
		
		
	}
	
	public void doPost(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {
		doGet(request, response);
	}
}
