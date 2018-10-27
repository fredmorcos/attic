<%@ page import="java.util.*" %>

<html>
<head>
	<title>Welcome to MovieDB</title>
</head>

<body>

<h2>Welcome to MovieDB!</h2>

<Form action="LoginServlet">
	<b> To Edit your account information : </b> 
	
	<br><input type='hidden' name='toDo' value="editAccountInfo"/>
	<input type='submit' name='editInfo' value='Edit'/>
</Form>

<Form action="SearchServlet">
	<b> To search a movie by : </b> 
	<br>
	Movie name: <input type='text' name="movieName" />
	<input type='hidden' name='toDo' value="title"/>
	<input type='submit' name='search' value="Search"/>
</Form>

<Form action="SearchServlet">
	Country name: <input type='text' name="countryName" />
	<input type='hidden' name='toDo' value="country"/>
	<input type='submit' name='search' value="Search"/>
</Form>

<Form action="SearchServlet">
	Actor name: <input type='text' name="actorName" />
	<input type='hidden' name='toDo' value="actor"/>
	<input type='submit' name='search' value="Search"/>
</Form>

<Form action="SearchServlet">
	Actress name: <input type='text' name="actressName" />
	<input type='hidden' name='toDo' value="actress"/>
	<input type='submit' name='search' value="Search"/>
</Form>

<Form action="SearchServlet">
	Language: <input type='text' name="languageName" />
	<input type='hidden' name='toDo' value="language"/>
	<input type='submit' name='search' value="Search"/>
</Form>


<table border="2">
<tr>
		<td>Title</td><td>Year</td>
		<td>Language</td><td>Country</td>
		<td>Actors</td><td>Actresses</td>
		<td>Director</td>
	</tr>
<% 	List titleList = (LinkedList) request.getAttribute("titleList");
	List yearList = (LinkedList) request.getAttribute("yearList");
	List languageList = (LinkedList) request.getAttribute("languageList");
	List countryList = (LinkedList) request.getAttribute("countryList");
	List actorList = (LinkedList) request.getAttribute("actorList");
	List actressList = (LinkedList) request.getAttribute("actressList");
	List directorList = (LinkedList) request.getAttribute("directorList");
	if(titleList!=null && yearList!=null && languageList!=null && countryList!=null && actorList!=null && actressList!=null && directorList!=null) 
	{ 
		Iterator iter1 = titleList.iterator();
		Iterator iter2 = yearList.iterator();
		Iterator iter3 = languageList.iterator();
		Iterator iter4 = countryList.iterator();
		Iterator iter5 = actorList.iterator();
		Iterator iter6 = actressList.iterator();
		Iterator iter7 = directorList.iterator();
		while(iter1.hasNext() && iter2.hasNext() && iter3.hasNext() && iter4.hasNext() && iter5.hasNext() && iter6.hasNext() && iter7.hasNext() ) 
		{
%>
			<tr>
			<td><%= (String) iter1.next() %></td>
			<td><%= (String) iter2.next() %></td>
			<td><%= (String) iter3.next() %></td>
			<td><%= (String) iter4.next() %></td>
			<td><%= (String) iter5.next() %></td>
			<td><%= (String) iter6.next() %></td>
			<td><%= (String) iter7.next() %></td>
			</tr>		
<%      }   %>
<%	} %>
	<tr>
	<!-- movies here! -->
	</tr>
</table>

<Form action="index.html">
	<b> To Log Out : </b> 
	<br>
	<input type='submit' name='logoutButton' value="LogOut"/>
</Form>
<!--
<% 	String message = (String) request.getAttribute("message");
    if (message != null) { %>
	<font color='red'><%= "Sorry. There has been an error: " + message %></font>
<% } %>

<%
	String text1 = (String) request.getAttribute("txt1");
	if(text1 == null) text1 = "";
	String text2 = (String) request.getAttribute("txt2");
	if(text2 == null) text2 = "";
	String text3 = (String) request.getAttribute("txt3");
	if(text3 == null) text3 = "";
	String text4 = (String) request.getAttribute("txt4");
	if(text4 == null) text4 = "";
%>

<br><b>Displaying text:</b><br>
<Form action="ControllerServlet">
	TextField1: <input type='text' name='textField1' value='<%=text1%>'/>
	TextField2: <input type='text' name='textField2' value='<%=text2%>'/>
	<input type='hidden' name='todo' value='showText'/>
	<input type='submit' value='Display'/>
</Form>

<br><b>Adding two integer values:</b><br>
<Form action="ControllerServlet">
	First Number: <input type='text' name='textField3' value='<%=text3%>'/>
	Second Number: <input type='text' name='textField4' value='<%=text4%>'/>
	<input type='hidden' name='todo' value='calculateNumber'/>
	<input type='submit' value='Add'/>
</Form>
<br>

<% String action = (String) request.getAttribute("action");
if(action == null) {
	// do nothing
} else if(action.compareTo("showText") == 0) {
%>
	<%= "You entered the following: " + text1 + " " + text2 %>
<% } else if(action.compareTo("addNumbers") == 0) { 
	Integer i = (Integer) request.getAttribute("result");
		
		if (i != null) {
%>
		<b> <%= "Result is: " + i.toString()%> </b>
	<% } %>
<% } %>

<br><br>

<b>Let me say this:</b><br>
<table border="1">
<% List myList = (LinkedList) request.getAttribute("theList");
	if(myList != null) {
		Iterator iter = myList.iterator();
		while(iter.hasNext()) {
%>
			<tr><td><%= (String) iter.next() %></td></tr>		
<%      }   %>
<%	} %>
</table> -->
</body>
</html>