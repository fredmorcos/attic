from sqlalchemy                 import Column, Integer, Text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm             import scoped_session, sessionmaker
from zope.sqlalchemy            import ZopeTransactionExtension

DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension()))
Base      = declarative_base()

class User(Base):
    __tablename__ = 'users'
    id            = Column(Integer , primary_key=True)
    username      = Column(Text    , unique=True)
    email         = Column(Text    , unique=True)
    password      = Column(Text)
    first_name    = Column(Text)
    last_name     = Column(Text)

    def __init__(self, username):
        self.username   = username
        self.email      = None
        self.password   = None
        self.first_name = None
        self.last_name  = None
