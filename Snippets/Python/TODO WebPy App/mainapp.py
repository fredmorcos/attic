import web, hashlib, sqlite3

render = web.template.render('templates/')

urls = (
	'/', 'index',
	'/login', 'login',
	'/registerpage', 'registerpage',
	'/register', 'register',
	'/todospage', 'todospage',
	'/logout', 'logout',
	'/addtodo', 'addtodo',
	'/search', 'search'
)

web.config.debug = False
app = web.application(urls, globals())
session = web.session.Session(app, web.session.DiskStore('sessions'))

class search:
	def POST(self):
		i = web.input()
		db = sqlite3.connect('db/sqlitedb')
		query = 'select * from users where username like "%%%s%%";' % (i.term)
		res = db.execute(query)
		results = []
		for r in res:
			results += [r[1]]
		return render.search(results)

class addtodo:
	def POST(self):
		if session.get('logged_in', False):
			i = web.input()
			db = sqlite3.connect('db/sqlitedb')
			username = session.get('username', '')
			if username == '':
				raise web.seeother('/')
			query = 'select * from users where username = "%s";' % (username)
			res = db.execute(query)
			userid = None
			for user in res:
				if user[1] == username:
					userid = user[0]
			if userid == None:
				raise web.seeother('/')
			query = 'insert into todos (userid,text) values (%s,"%s");' % (str(userid), i.todotext)
			db.execute(query)
			db.commit()
		raise web.seeother('/todospage')

class logout:
	def GET(self):
		session.logged_in = False
		session.kill()
		raise web.seeother('/')

class todospage:
	def GET(self):
		if session.get('logged_in', False):
			db = sqlite3.connect('db/sqlitedb')
			username = session.get('username', '')
			if username == '':
				raise web.seeother('/')
			query = 'select * from users where username = "%s";' % (username)
			res = db.execute(query)
			userid = None
			for user in res:
				if user[1] == username:
					userid = user[0]
			if userid == None:
				raise web.seeother('/')
			query = 'select * from todos where userid = %s;' % (str(userid))
			res = db.execute(query)
			todos = []
			for todo in res:
				todos += [todo[1]]
			return render.todospage(todos)
		else:
			raise web.seeother('/')

class index:
	def GET(self):
		if session.get('logged_in', False):
			web.seeother('/todospage')
		return render.index(False)

class login:
	def POST(self):
		i = web.input()
		passhash = hashlib.sha512(i.password).hexdigest()
		db = sqlite3.connect('db/sqlitedb')
		query = 'select * from users where username = "%s" and password = "%s";' % (i.username, passhash)
		res = db.execute(query)
		for user in res:
			if user[1] == i.username and user[2] == passhash:
				session.logged_in = True
				session.username = i.username
				raise web.seeother('/todospage')
		raise web.seeother('/')

class registerpage:
	def GET(self):
		return render.registerpage(False)

class register:
	def POST(self):
		i = web.input()
		if i.username == '' or i.password == '':
			return render.registerpage(True)
		db = sqlite3.connect('db/sqlitedb')
		res = db.execute('select * from users;')
		for user in res:
			if user[1] == i.username:
				return render.registerpage(True)
		newpass = hashlib.sha512(i.password).hexdigest()
		query = 'insert into users (username,password) values ("%s", "%s");' % (i.username, newpass)
		db.execute(query)
		db.commit()
		raise web.seeother('/')

if __name__ == '__main__':
	app.run()
