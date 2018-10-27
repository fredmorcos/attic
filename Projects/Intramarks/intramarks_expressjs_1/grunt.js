/*global module:false*/
module.exports = function (grunt) {
    var banner = ['/*!', '* <%= pkg.name %> - v<%= pkg.version %> - <%= grunt.template.today("yyyy-mm-dd") %>', '<%= pkg.homepage ? "* " + pkg.homepage : "" %>', '* Copyright (c) <%= grunt.template.today("yyyy") %> <%= pkg.author.name %>;', '* Licensed <%= _.pluck(pkg.licenses, "type").join(", ") %>', '*/'].join('\n');

    grunt.initConfig({
        pkg: '<json:package.json>',
        meta: {
            banner: banner
        },
        lint: {
            files: ['app/core/*.js', '*.js', '*.json']
        },
        watch: {
            files: '<config:lint.files>',
            tasks: 'lint'
        },
        beautify: {
            files: '<config:lint.files>'
        },
        beautifier: {
            options: {
                indentSize: 4
            }
        },
        jshint: {
            options: {
                curly: true,
                eqeqeq: true,
                immed: true,
                latedef: true,
                newcap: true,
                noarg: true,
                sub: true,
                undef: true,
                boss: true,
                eqnull: true,
                browser: true
            },
            globals: {
                jQuery: true,
                require: true,
                module: true,
                exports: true,
                console: true,
                process: true,
                __dirname: true
            }
        }
    });

    grunt.loadNpmTasks('grunt-beautify');

    // Default task.
    grunt.registerTask('default', 'beautify lint');
};
