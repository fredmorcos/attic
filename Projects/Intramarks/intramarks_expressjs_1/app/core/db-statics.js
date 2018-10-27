exports.initialize = function (app, mongoose){

    app.userSchema = mongoose.Schema({
        username:  { type: String, unique: true },
        password:  String,
        email:     { type: String, unique: true },
        verified:  { type: Boolean, 'default': false},
        bookmarks: Array
    });

    app.userSchema.statics.find_match = function (email, pass, cb) {
      this.find({ email: email, password: pass }, cb);
    };

    app.userSchema.statics.username_exists = function (val, cb) {
      this.find({ username: val }, cb);
    };

    app.userSchema.statics.email_exists = function (val, cb) {
      this.find({ email: val }, cb);
    };

    app.UserModel = mongoose.model('UserModel', app.userSchema);
};
