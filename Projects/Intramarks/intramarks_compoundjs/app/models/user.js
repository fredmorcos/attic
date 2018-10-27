module.exports = function (compound) {
    function User () {
        this.username = '';
        this.password = '';
    }

    User.prototype.method = function () {
        return 'I am a User model!';
    };

    compound.models.User = User;
    User.className = 'User';
};
