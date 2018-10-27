/*
 * Localization module
 */

exports.say = function (what) {
    return {
        'username_exists': 'Username already in use.',
        'email_exists': 'Email already in use.',
        'register_success': 'Please check your email inbox.',
        'login_failed': 'Login failed.',
        'login_success': 'Login success.',
        'invalid_email': 'Please enter a valid email address.',
        'invalid_email_len': 'Your email address should be shorter than 65 character and longer than 6.',
        'invalid_username_len': 'Your username should be shorter than 16 characters and longer than 3.',
        'invalid_password_len': 'Your password cannot be shorter than 5 characters.'
    }[what];
};
