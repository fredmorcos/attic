from pyramid.response import Response
from pyramid.view import view_config
from sqlalchemy.exc import DBAPIError
from markr.models import DBSession, User

@view_config(route_name='index', renderer='index.mako')
def index_view(request):
    return {}
    # try:
    #     one = DBSession.query(MyModel).filter(MyModel.name == 'one').first()
    # except DBAPIError:
    #     return Response(conn_err_msg, content_type='text/plain', status_int=500)
    # return {'one': one, 'project': 'Markr'}
