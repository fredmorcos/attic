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


public class LoginServlet extends HttpServlet {

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		HttpSession session = request.getSession();
		
		
		
		/* get the username and password from the login form */
		
		String username = (String)(request.getParameter("textUsername"));
		String password = (String)(request.getParameter("textPassword"));
		
		System.out.println("LoginServlet: received request " + username + ", " + password);
		String toDo = (String)(request.getParameter("toDo"));
		String finishEditing =(String)(request.getParameter("finishEditing"));
		if (toDo==null && finishEditing==null)
		{
		if(username==""  || password=="")
		{
			String url = "index.html";

			// initiate dispatcher and call the url
			// forwarding request- and response attributes
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
			
		}
		else
	{
		if(LoginChecker.checkLogin(username, password))
		{
			
			String url = "mainPage.jsp";

			// initiate dispatcher and call the url
			// forwarding request- and response attributes
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
			session.setAttribute("sessionUsername", username);
			//String sessionUsername = (String) session.getAttribute("username");
		}
		else
		{
			String url = "index.html";

			// initiate dispatcher and call the url
			// forwarding request- and response attributes
			RequestDispatcher dispatcher;
			dispatcher = request.getRequestDispatcher(url);
			dispatcher.forward(request, response);
			
			
		}
	}
		}
		else
			if(finishEditing!=null)
		{
				//do the edit
				String newPassword= (String)(request.getParameter("newPassword"));
				String newAddress= (String)(request.getParameter("newAddress"));
				String newEmail= (String)(request.getParameter("newEmail"));
				String newFname= (String)(request.getParameter("newFname"));
				String newLname= (String)(request.getParameter("newLname"));
				String sessionUsername = (String) (session.getAttribute("sessionUsername"));
				edit.update_account(sessionUsername,newPassword, newEmail,newAddress,newFname,newLname);
				
				System.out.println(" MY SESSION SAVED USERNAME IS : "+ sessionUsername );
				
				String url = "mainPage.jsp";
				RequestDispatcher dispatcher;
				dispatcher = request.getRequestDispatcher(url);
				dispatcher.forward(request, response);
				
		}
			else 
			{
				String sessionUsername = (String) (session.getAttribute("sessionUsername"));
				GetUserInfo.getInfo(sessionUsername);
				System.out.println(" MY SESSION SAVED USERNAME IS : "+ sessionUsername );
				System.out.println("PASSWORD IS "+GetUserInfo.getPassword());
				System.out.println("ADDRESS IS "+GetUserInfo.getAddress());
				System.out.println("EMAIL IS "+GetUserInfo.getEmail());
				System.out.println("1stName IS "+GetUserInfo.getFname());
				System.out.println("last Name IS "+GetUserInfo.getLname());
				request.setAttribute("password",GetUserInfo.getPassword());
				request.setAttribute("email",GetUserInfo.getEmail());
				request.setAttribute("address",GetUserInfo.getAddress());
				request.setAttribute("fname",GetUserInfo.getFname());
				request.setAttribute("lname",GetUserInfo.getLname());
				String url = "editInfo.jsp";
				RequestDispatcher dispatcher;
				dispatcher = request.getRequestDispatcher(url);
				dispatcher.forward(request, response);
			}
		}

	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// doPost shall do the same as doGet
		// in this case it doesn't matter
		// where the code is implemented (in doPost or doGet)
		doGet(request, response);
	}
}