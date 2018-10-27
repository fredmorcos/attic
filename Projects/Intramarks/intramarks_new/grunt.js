/*global module:false*/
module.exports = function(grunt) {

  grunt.initConfig({
    pkg: '<json:package.json>',
    meta: {
      banner: '/*! <%= pkg.title || pkg.name %> - v<%= pkg.version %> - ' + '<%= grunt.template.today("yyyy-mm-dd") %>\n' + '<%= pkg.homepage ? "* " + pkg.homepage + "\n" : "" %>' + '* Copyright (c) <%= grunt.template.today("yyyy") %> <%= pkg.author.name %>;' + ' Licensed <%= _.pluck(pkg.licenses, "type").join(", ") %> */'
    },
    build: {
      dest: 'public'
    },
    jade: {
      templates: {
        src: ['app/views/*.jade'],
        dest: '<%= build.dest %>/',
        options: {
          data: {
            debug: false,
            title: 'Welcome To Express (Build with Grunt)',
            appcache: null,
            size: 'smallalt'
          },
          pretty: true,
          compileDebug: true,
          client: false // to output HTML files instead of DEFAULT JS FILES
        }
      },
      ajax: {
        src: ['app/views/_ajax/*.jade'],
        dest: '<%= build.dest %>/_ajax/',
        options: {
          data: {
            debug: false,
            title: 'Welcome To Express (Build with Grunt)',
            appcache: null,
            size: 'smallalt'
          },
          pretty: true,
          compileDebug: true,
          client: false // to output HTML files instead of DEFAULT JS FILES
        }
      },
      fixtures: {
        src: ['app/views/_fixtures/*.jade'],
        dest: '<%= build.dest %>/ws/',
        options: {
          pretty: true,
          compileDebug: true,
          client: false,
          // to output HTML files instead of DEFAULT JS JADE FILES
          extension: '.js'
        }
      }
    },
    less: {
      build: {
        options: {
          compress: false
        },
        files: {
          '<%= build.dest %>/stylesheets/app.css': 'app/assets/stylesheets/less/app.less',
          '<%= build.dest %>/stylesheets/theme.css': 'app/assets/stylesheets/less/theme.less',
          '<%= build.dest %>/stylesheets/grey-theme.css': 'app/assets/stylesheets/less/grey-theme.less',
          '<%= build.dest %>/stylesheets/dark-theme.css': 'app/assets/stylesheets/less/dark-theme.less',
          '<%= build.dest %>/stylesheets/ie.css': 'app/assets/stylesheets/less/ie.less'
        }
      },
      release: {
        options: {
          compress: true
        },
        files: {
          '<%= build.dest %>/stylesheets/app.min.css': 'app/assets/stylesheets/less/app.less',
          '<%= build.dest %>/stylesheets/theme.min.css': 'app/assets/stylesheets/less/theme.less',
          '<%= build.dest %>/stylesheets/grey-theme.min.css': 'app/assets/stylesheets/less/grey-theme.less',
          '<%= build.dest %>/stylesheets/dark-theme.min.css': 'app/assets/stylesheets/less/dark-theme.less',
          '<%= build.dest %>/stylesheets/ie.min.css': 'app/assets/stylesheets/less/ie.less'
        }
      }
    },
    lint: {
      files: ['grunt.js', 'app/assets/javascripts/**/*.js', 'test/js/**/*.js']
    },
    qunit: {
      files: ['test/*.html']
    },
    concat: {
      mandatory: {
        src: ['app/assets/vendor/modernizr/*.js'],
        dest: '<%= build.dest %>/javascripts/libs/modernizr.js'
      },
      single1: {
        src: ['app/assets/vendor/json2/*.js'],
        dest: '<%= build.dest %>/javascripts/libs/json2.js'
      },
      single2: {
        src: ['app/assets/vendor/jquery.color/*.js'],
        dest: '<%= build.dest %>/javascripts/libs/jquery.color.js'
      },
      loca: {
        src: ['app/assets/javascripts/app.settings.js'],
        dest: '<%= build.dest %>/javascripts/app.settings.js'
      },
      libs: {
        src: ['app/assets/vendor/jquery/*.js', 'app/assets/vendor/underscore/*.js', 'app/assets/vendor/jquery.tools.validator/*.js', 'app/assets/vendor/jquery.ui/*.js', 'app/assets/vendor/jquery.bxslider/*.js', 'app/assets/vendor/jquery.sharrre/*.js', 'app/assets/vendor/jquery.encrypt/*.js', 'app/assets/javascripts/libs/digitas.jquery.transition.js', 'app/assets/javascripts/libs/digitas.reflow.js', 'app/assets/javascripts/libs/digitas.reflow.popin.js', 'app/assets/javascripts/libs/digitas.reflow.popin-controller.js', 'app/assets/javascripts/libs/digitas.reflow.forms.js', 'app/assets/javascripts/libs/jquery.helpers.js', 'app/assets/javascripts/libs/nissan.custom-tipper.js'],
        dest: '<%= build.dest %>/javascripts/libs/libs.js'
      },
      dist: {
        src: ['<banner:meta.banner>', 'app/assets/javascripts/social.js', 'app/assets/javascripts/app.js', 'app/assets/javascripts/libs/nissan.keep-in-touch.js', 'app/assets/javascripts/libs/nissan.social-features.js', 'app/assets/javascripts/libs/nissan.tracking.js', 'app/assets/javascripts/libs/nissan.accessories.js', 'app/assets/javascripts/libs/nissan.lightbox.js', 'app/assets/javascripts/libs/nissan.complexslider.js', 'app/assets/javascripts/libs/nissan.editoslider.js', 'app/assets/javascripts/libs/nissan.to-top.js', 'app/assets/javascripts/libs/nissan.anchor-nav.js', 'app/assets/javascripts/libs/nissan.compare-grade.js', 'app/assets/javascripts/libs/nissan.compare-selection.js', 'app/assets/javascripts/libs/nissan.compare-configuration.js', 'app/assets/javascripts/libs/nissan.engine.js', 'app/assets/javascripts/libs/nissan.prices-specs.js', 'app/assets/javascripts/libs/nissan.nav-slider.js', 'app/assets/javascripts/app.init.js'],
        dest: '<%= build.dest %>/javascripts/app.js'
      }
    },
    min: {
      mandatory: {
        src: ['<config:concat.mandatory.dest>'],
        dest: '<%= build.dest %>/javascripts/libs/modernizr.min.js'
      },
      libs: {
        src: ['<config:concat.libs.dest>'],
        dest: '<%= build.dest %>/javascripts/libs/libs.min.js'
      },
      dist: {
        src: ['<banner:meta.banner>', '<config:concat.dist.dest>'],
        dest: '<%= build.dest %>/javascripts/app.min.js'
      }
    },
    usemin: {
      html: ['<%= build.dest %>/**/*.html'],
      css: ['<%= build.dest %>/stylesheets/**/*.css']
    },
    copy: {
      robots: {
        files: {
          '<%= build.dest %>/robots.txt': 'app/assets/robots/robots.txt'
        }
      },
      icons: {
        files: {
          '<%= build.dest %>/': 'app/assets/ico/*.png',
          '<%= build.dest %>/favicon.ico': 'app/assets/ico/*.ico'
        }
      },
      images: {
        files: {
          '<%= build.dest %>/images/': 'app/assets/images/**/*'
        }
      },
      flash: {
        files: {
          '<%= build.dest %>/flash/': 'app/assets/flash/**/*'
        }
      },
      fonts: {
        files: {
          '<%= build.dest %>/fonts/': 'app/assets/fonts/**/*'
        }
      },
      test: {
        files: {
          '<%= build.dest %>/test/': ['test/**']
        }
      }
    },
    watch: {
      js: {
        files: '<config:lint.files>',
        tasks: 'lint qunit concat copy:test'
      },
      less: {
        files: ['app/assets/stylesheets/less/**/*.less'],
        tasks: 'less:build'
      },
      jade: {
        files: ['app/views/**/*.jade'],
        tasks: 'jade'
      },
      vendor: {
        files: ['app/assets/vendor/**/*.js'],
        tasks: 'copy:build'
      },
      css: {
        files: ['app/assets/vendor/**/*.css'],
        tasks: 'copy:build'
      }
      //,
      // img: {
      //     files: ['app/assets/images/**/*'],
      //     tasks: 'copy:build'
      // },
      // videos: {
      //     files: ['app/assets/videos/**/*'],
      //     tasks: 'copy:build'
      // },
      // flash: {
      //     files: ['app/assets/flash/**/*'],
      //     tasks: 'copy:build'
      // },
      // fonts: {
      //     files: ['app/assets/fonts/**/*'],
      //     tasks: 'copy:build'
      // }
    },
    clean: {
      build: ['public']
    },
    jshint: {
      options: {
        devel: false,
        curly: true,
        eqeqeq: true,
        immed: false,
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
        $: true,
        _: true,
        Modernizr: true,
        require: true,
        module: true,
        exports: true,
        console: true,
        process: true,
        __dirname: true
      }
    },
    uglify: {
      options: {
        beautify: true,
        indent_start: 0,
        indent_level: 2
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-compress');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-jade');
  grunt.loadNpmTasks('grunt-usemin');

  grunt.registerTask('default', ['lint', 'clean:build', 'copy', 'jade', 'concat', 'less:build']);
  grunt.registerTask('build', 'default');
  grunt.registerTask('release', ['build', 'less:release', 'min']); // 'usemin:html'
};
