import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import businessLogic.ListGenerator;

public class ControllerServlet extends HttpServlet {

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		//HttpSession session = request.getSession();
		//session.setAttribute("username", username);
		//String username = (String) session.getAttribute("username");
		
		// toDo-parameter set by jsp-page
		// defines the action to be taken by controller
		String toDo = request.getParameter("todo");

		// Controller checks the content of the
		// toDo-parameter to find out what it should do

		// standard - parameter is empty
		/*
		if (toDo == null) {
			toDo = "";
		}

		// toDo = showText
		else if (toDo.compareTo("showText") == 0) {
			// read parameters set by JSP
			String firstText = request.getParameter("textField1");
			String secondText = request.getParameter("textField2");

			// data to be passed to the JSP page
			request.setAttribute("txt1", firstText);
			request.setAttribute("txt2", secondText);

			// define the action to be taken by the JSP
			request.setAttribute("action", "showText");
		}

		// toDo = calculateNumber
		else if (toDo.compareTo("calculateNumber") == 0) {
			String firstNum = request.getParameter("textField3");
			String secondNum = request.getParameter("textField4");

			System.out.println("PRINTING OUT: " + firstNum + secondNum);

			// calculate stuff
			try {
				int num1 = Integer.parseInt(firstNum);
				int num2 = Integer.parseInt(secondNum);
				int r = num1 + num2;

				Integer result = new Integer(r);

				request.setAttribute("result", result);
			} catch (NumberFormatException n) {
				// set error message
				request.setAttribute("message",
						"Incorrect input. You must enter a number");
			}

			request.setAttribute("text3", firstNum);
			request.setAttribute("text4", secondNum);

			request.setAttribute("action", "addNumbers");
		}
*/
		// Forwarding a list
		//List myList = ListGenerator.giveMeMyList();
		//request.setAttribute("theList", myList);

		// define the url that shall be called
		// in the next step
		/*String url = "mainPage.jsp";

		// initiate dispatcher and call the url
		// forwarding request- and response attributes
		RequestDispatcher dispatcher;
		dispatcher = request.getRequestDispatcher(url);
		dispatcher.forward(request, response); */
	}

	public void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// doPost shall do the same as doGet
		// in this case it doesn't matter
		// where the code is implemented (in doPost or doGet)
		doGet(request, response);
	}
}