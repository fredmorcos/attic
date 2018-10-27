<%@ page import="java.util.*" %>

<html>
<head>
	<title>Edit Account Information</title>
</head>

<body>

	<br><b>Displaying text:</b><br>
<Form action="LoginServlet">
	Password: <input type='text' name="newPassword" value=<%= (String) request.getAttribute("password") %>  />
	<br>
	Address: <input type='text' name="newAddress" value=<%= (String) request.getAttribute("address") %>  />
	<br>
	email: <input type='text' name="newEmail" value=<%= (String) request.getAttribute("email") %>   />
	<br>
	First name: <input type='text' name="newFname" value=<%= (String) request.getAttribute("fname") %>  />
	<br>
	Last name: <input type='text' name="newLname" value=<%= (String) request.getAttribute("lname") %>  />
	<input type='hidden' name='finishEditing' value="doneEditing"/>
	<br>
	<input type='submit' name='edit' value='Edit'/>
</Form>

</body>
</html>
