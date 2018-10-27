import os
import logging
import sqlite3

from pyramid.config import Configurator
from pyramid.session import UnencryptedCookieSessionFactoryConfig
from pyramid.events import NewRequest
from pyramid.events import subscriber
from pyramid.events import ApplicationCreated
from pyramid.exceptions import NotFound
from pyramid.httpexceptions import HTTPFound
from pyramid.view import view_config

from wsgiref.simple_server import make_server

logging.basicConfig()
log = logging.getLogger(__file__)

here = os.path.dirname(os.path.abspath(__file__))

@view_config(route_name='list', renderer='list.mako')
def list_view(request):
    rs = request.db.execute('select id, name from tasks where closed = 0')
    tasks = [dict(id=row[0], name=row[1]) for row in rs.fetchall()]
    return {'tasks': tasks}

@view_config(route_name='new', renderer='new.mako')
def new_view(request):
    if request.method == 'POST':
        if request.POST.get('name'):
            request.db.execute(
                'insert into tasks (name, closed) values (?, ?)',
                [request.POST['name'], 0])
            request.db.commit()
            request.session.flash('New task added!')
            return HTTPFound(location=request.route_url('list'))
        else:
            request.session.flash('Please enter a name for the task!')
    return {}

@view_config(route_name='close')
def close_view(request):
    task_id = int(request.matchdict['id'])
    request.db.execute('update tasks set closed = ? where id = ?',
                       (1, task_id))
    request.db.commit()
    request.session.flash('Task was closed!')
    return HTTPFound(location=request.route_url('list'))

@view_config(context='pyramid.exceptions.NotFound', renderer='notfound.mako')
def notfound_view(self):
    return {}

@subscriber(ApplicationCreated)
def app_created_sub(event):
    log.warn('Init DB...');
    with open(os.path.join(here, 'schema.sql')) as f:
        statements = f.read()
        settings = event.app.registry.settings
        db = sqlite3.connect(settings['db'])
        db.executescript(statements)
        db.commit()

@subscriber(NewRequest)
def new_req_sub(event):
    request = event.request
    settings = request.registry.settings
    request.db = sqlite3.connect(settings['db'])
    request.add_finished_callback(close_db_conn)

def close_db_conn(request):
    request.db.close()

if __name__ == '__main__':
    # config settings
    settings = {}
    settings['reload_all'] = True
    settings['debug_all'] = True
    settings['db'] = os.path.join(here, 'tasks.db')
    settings['mako.directories'] = os.path.join(here, 'templates')
    # session init
    session_factory = UnencryptedCookieSessionFactoryConfig('secret')
    # config init
    config = Configurator(settings=settings, session_factory=session_factory)
    # config routes
    config.add_route('list',  '/')
    config.add_route('new',   '/new')
    config.add_route('close', '/close/{id}')
    # config final
    config.add_static_view('static', os.path.join(here, 'static'))
    config.scan()
    # server
    app = config.make_wsgi_app()
    server = make_server('localhost', 8000, app)
    server.serve_forever()
