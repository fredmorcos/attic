# TODO 1: Implement an ini parser to read settings for different
# profiles (development, testing, production, etc...). Also don't
# forget to implement profile inheritance.

from flask import Flask
from flask import render_template, url_for, flash
from flask import request, redirect, session

from mongokit import Connection, Document

# FIXME: add these settings to profile configuration
# MongoDB Configuration
MONGODB_HOST = 'localhost'
MONGODB_PORT = 27017

# Flask app initialization
app = Flask(__name__)
app.config.from_object(__name__)

# MongoDB Connection
connection = Connection(app.config["MONGODB_HOST"],
                        app.config["MONGODB_PORT"])

class MinLength(object):
    def __init__(self, size):
        self.size = size

    def __call__(self, value):
        if (len(value) < size):
            # raise ValidationError("%s is too short")
            return False
        else:
            return True

class MaxLength(object):
    def __init__(self, size):
        self.size = size

    def __call__(self, value):
        if (len(value) > size):
            # raise ValidationError("%s is too long")
            return False
        else:
            return True

@connection.register
class User(Document):
    # FIXME: set this in the profile settings
    __database__ = "markr"
    __collection__ = "users"

    structure = {
        "username": basestring,
        "email": basestring,
        "password": basestring
        }
    validators = {
        "username": [MinLength(6), MaxLength(12)]
        }

@app.route("/logout", methods=["POST"])
def logoutPost():
    # FIXME: logs out user if logged in
    return redirect(url_for("index"))

@app.route("/bookmark", methods=["POST"])
def bookmarkPost():
    # FIXME: check whether the user is logged in
    # FIXME: do some checks on whether the url matches the url regex
    # FIXME: implement adding bookmark to user database
    return redirect(url_for("index"))

@app.route("/register", methods=["GET"])
def registerGet():
    return render_template("register.html")

@app.route("/register", methods=["POST"])
def registerPost():
    # FIXME: when the database backend is available, check whether the
    # username is already in use and add the user into the database
    return redirect(url_for("index"))

@app.route("/login", methods=["GET"])
def loginGet():
    # FIXME: when templates are available, return the login page with
    # login form.
    return render_template("login.html")

@app.route("/login", methods=["POST"])
def loginPost():
    # FIXME: check user credentials, if they're fine, then return the
    # user to the index page, if not, return the user to the loginGet
    # page with his inserted data.
    return redirect(url_for("index"))

@app.route("/")
def index():
    # FIXME: when templates are available, return the main page. if
    # there is a logged user, make sure to return him to his home
    # page.
    return render_template("index.html")

if __name__ == "__main__":
    user = connection.User()
    user["username"] = "foo"
    user.save()

    # FIXME: when TODO 1 is implemented, add a debug switch and use it
    # here.
    # FIXME: when the debug switch is False, use app.run("0.0.0.0")
    app.debug = True
    # FIXME: use /dev/urandom and sha512sum to generate a secret key
    # FIXME: put the secret key an option in the profile settings
    app.secret_key = "foobar"
    app.run()
