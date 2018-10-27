<%@ page import="java.util.*" %>

<html>
<head>
	<title>Main Test Page</title>
</head>

<body>

<h2>Welcome to Move Db</h2>

 	<br><b>Displaying text:</b><br>
<Form action="RegisterServlet">
	Username: <input type='text' name="textUsername" /><br>
	Password: <input type='password' name="textPassword" /><br>
	Address: <input type='text' name="textAddress" />
	email: <input type='text' name="textEmail" />
	First name: <input type='text' name="fname" />
	Last name: <input type='text' name="lname" />
	<input type='hidden' name='todo' value="registerPage"/>
	<input type='submit' name='display' value='Display'/>
</Form>

</body>
</html>
