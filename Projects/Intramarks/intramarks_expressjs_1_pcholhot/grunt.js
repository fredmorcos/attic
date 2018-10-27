/*global module:false*/
module.exports = function(grunt) {
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
        docco: {
            project_name: '<%= pkg.name %>',
            files: ['app/resources/scripts/*.js']
        },
        styleguide: {
            styledocco: {
                options: {
                    framework: {
                        name: 'styledocco'
                    },
                    name: 'Style Guide',
                    template: {
                        include: ['plugin.css', 'app.js']
                    }
                },
                files: {
                    'docs/css': 'app/resources/sass/*.scss'
                }
            }
        },
        shell: {
            install_modules: {
                command: 'npm install'
            },
            clean_modules: {
                command: 'rm -rf node_modules/'
            },
            make_db: {
                command: 'mkdir db'
            },
            clean_db: {
                command: 'rm -rf db/'
            },
            watchers: {
                command: 'sass --watch app/resources/sass:app/resources/css'
            },
            run_db: {
                command: 'mongod --smallfiles --dbpath db/'
            },
            run: {
                command: 'supervisor app.js'
            },
            _options: {
                stdout: true
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
                require: true,
                module: true,
                exports: true,
                console: true,
                process: true,
                __dirname: true
            }
        }
    });

    grunt.loadNpmTasks('grunt-shell');
    grunt.loadNpmTasks('grunt-beautify');
    grunt.loadNpmTasks('grunt-docco');
    grunt.loadNpmTasks('grunt-styleguide');

    grunt.registerTask('default', 'beautify lint');
    grunt.registerTask('docs', 'docco styleguide');

    grunt.registerTask('reset_db', 'shell:clean_db shell:make_db');
    grunt.registerTask('reset_modules', 'shell:clean_modules shell:install_modules');

    grunt.registerTask('setup', 'reset_db reset_modules');
    grunt.registerTask('update', 'reset_modules');

    grunt.registerTask('watchers', 'shell:watchers');
    grunt.registerTask('rundb', 'shell:run_db');
    grunt.registerTask('run', 'shell:run');
};