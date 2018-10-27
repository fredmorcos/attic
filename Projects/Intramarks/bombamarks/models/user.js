exports.User = User;
exports.UserDB = UserDB;

function User (username, password, email) {
    this.username  = username;
    this.password  = password;
    this.email     = email;
    this.sessionId = null;
    this.bookmarks = [];
}

function UserDB () {
    this.table = [];
}

UserDB.exceptions = {};
UserDB.exceptions.InvalidUsername = {};
UserDB.exceptions.InvalidPassword = {};
UserDB.exceptions.UsernameExists = {};
UserDB.exceptions.EmailExists = {};

UserDB.prototype.usernameExists = function (username) {
    return this.table.some(function (element, index, array) {
        return (username === element.username);
    });
};

UserDB.prototype.emailExists = function (email) {
    return this.table.some(function (element, index, array) {
        return (email === element.email);
    });
};

UserDB.prototype.checkCredentials = function (username, password) {
    return this.table.some(function (element, index, array) {
	return (username === element.username &&
	        password === element.password);
    });
};

UserDB.prototype.authenticate = function (username, password) {
    if (!this.usernameExists(user.username)) {
        throw UserDB.exceptions.InvalidUsername;
    }

    if (!this.checkCredentials(username, password)) {
        throw UserDB.exceptions.InvalidPassword;
    }
};

UserDB.prototype.addUser = function (user) {
    if (this.usernameExists(user.username)) {
        throw UserDB.exceptions.UsernameExists;
    }

    if (this.emailExists(user.email)) {
        throw UserDB.exceptions.EmailExists;
    }

    this.table.push(user);
};
