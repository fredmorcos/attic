/*global module:false */
module.exports = function(grunt) {
  // Project configuration.
  grunt.initConfig({
    pkg: '<json:package.json>',
    meta: {
      banner: '/*! <%= pkg.name %> <%= grunt.template.today("yyyy-mm-dd") %> */\n'
    },
    build: {
      dest: 'public'
    },
    jade: {
      compile: {
        options: {
          pretty: true,
          data: {
            debug: false,
            appcache: null,
            title: "Made by Digitas"
          }
        },
        files: [{
          expand: true,
          cwd: "app/views/",
          src: ["*.jade"],
          dest: "public/",
          ext: ".html"
        },{
          expand: true,
          cwd: "app/views/common/",
          src: ["*.jade"],
          dest: "public/exported-blocks/",
          ext: ".html"
        }
        ]
      }
    },
    less: {
      build: {
        options: {
          compress: false
        },
        files: {
          '<%= build.dest %>/stylesheets/app.css': 'app/assets/stylesheets/app.less'
        }
      }
    },
    stylus: {
      build: {
        options: {
          compress: false
        },
        files: {
          '<%= build.dest %>/stylesheets/app.css': 'app/assets/stylesheets/app.styl'
        }
      }
    },
    jshint: {
      options: {
        devel: false,
        curly: true,
        eqeqeq: true,
        immed: false,
        latedef: true,
        newcap: false,
        noarg: true,
        sub: true,
        undef: true,
        boss: true,
        eqnull: true,
        browser: true,
        expr: true,
        globals: {
          Modernizr: true,
          jQuery: true,
          $: true
        }
      },
      files: ['package.json', 'Gruntfile.js', 'app/assets/javascripts/**/*.js', 'test/js/**/*.js']
    },
    qunit: {
      files: ['test/*.html']
    },
    concat: {
      modernizer: {
        src: ['app/assets/vendor/modernizr/*.js'],
        dest: '<%= build.dest %>/javascripts/libs/modernizr.js'
      },
      vendor: {
        src: [
          'app/assets/vendor/jquery/*.js',
          'app/assets/vendor/jquery.flexslider/*.js',
          'app/assets/vendor/mustache/*.js',
          'app/assets/vendor/kontext/*.js',
          'app/assets/vendor/avgrund/*.js',
          'app/assets/vendor/tweenmax/*.js'
        ],
        dest: '<%= build.dest %>/javascripts/libs/vendor.js'
      },
      application: {
        src: [
          '<banner:meta.banner>',
          'app/assets/javascripts/app.js',
          'app/assets/javascripts/app-transition.js',
          'app/assets/javascripts/app-kontext.js',
          'app/assets/javascripts/app-push-loader.js',
          'app/assets/javascripts/app-touch.js',
          'app/assets/javascripts/app-modals.js',
          'app/assets/javascripts/app-carousel.js'
        ],
        dest: '<%= build.dest %>/javascripts/app.js'
      }
    },
    uglify: {
      // options: {
      //   compress: true,
      //   beautify: false,
      //   preserveComments: false,
      //   banner: '<%= meta.banner %>',
      //   mangle: false
      // },
      modernizer: {
        files: {
          'public/javascripts/libs/modernizr.min.js': ['app/assets/vendor/modernizr/*.js']
        }
      },
      vendor: {
        files: {
          'public/javascripts/libs/vendor.min.js': [
            'app/assets/vendor/jquery/*.js',
            'app/assets/vendor/jquery.flexslider/*.js',
            'app/assets/vendor/mustache/*.js',
            'app/assets/vendor/kontext/*.js',
            'app/assets/vendor/avgrund/*.js',
            'app/assets/vendor/tweenmax/*.js'
          ]
        }
      },
      application: {
        files: {
          'public/javascripts/app.min.js': [
            'app/assets/javascripts/app.js',
            'app/assets/javascripts/app-transition.js',
            'app/assets/javascripts/app-kontext.js',
            'app/assets/javascripts/app-push-loader.js',
            'app/assets/javascripts/app-touch.js',
            'app/assets/javascripts/app-modals.js',
            'app/assets/javascripts/app-carousel.js'
          ]
        }
      }
    },
    cssmin: {
      compress: {
        files: {
          'public/stylesheets/app.min.css': 'public/stylesheets/app.css'
        }
      }
    },
    // minify: {
    //   mandatory: {
    //     src: ['<config:concat.mandatory.dest>'],
    //     dest: '<%= build.dest %>/javascripts/libs/modernizr.min.js'
    //   },
    //   libs: {
    //     src: ['<config:concat.vendor.dest>'],
    //     dest: '<%= build.dest %>/javascripts/libs/libs.min.js'
    //   },
    //   dist: {
    //     src: ['<banner:meta.banner>', '<config:concat.main.dest>'],
    //     dest: '<%= build.dest %>/javascripts/app.min.js'
    //   }
    // },
    usemin: {
      html: ['<%= build.dest %>/**/*.html'],
      css: ['<%= build.dest %>/stylesheets/**/*.css']
    },
    copy: {
      icons: {
        files: [{
          expand: true,
          cwd: "app/assets/ico/",
          src: ["favicon.png"],
          dest: "public"
        }]
      },
      robots: {
        files: [{
          expand: true,
          cwd: "app/assets/robots/",
          src: ["*.txt"],
          dest: "public"
        }]
      },
      images: {
        files: [{
          expand: true,
          cwd: "app/assets/images/",
          src: ["**/*"],
          dest: "public/images/"
        }]
      },
      fonts: {
        files: [{
          expand: true,
          cwd: "app/assets/fonts/",
          src: ["**/*"],
          dest: "public/fonts/"
        }]
      },
      videos: {
        files: [{
          expand: true,
          cwd: "app/assets/videos/",
          src: ["**/*"],
          dest: "public/videos/"
        }]
      },
      flash: {
        files: [{
          expand: true,
          cwd: "app/assets/flash/",
          src: ["**/*"],
          dest: "public/flash/"
        }]
      },
      test: {
        files: [{
          expand: true,
          cwd: "test/",
          src: ["**/*"],
          dest: "public/test/"
        }]
      }
    },
    watch: {
      application: {
        files: ['<%= jshint.files %>'],
        tasks: ['jshint', 'concat']
      },
      less: {
        files: ['app/assets/stylesheets/**/*.less'],
        tasks: ['less:build']
      },
      stylus: {
        files: ['app/assets/stylesheets/**/*.styl'],
        tasks: ['stylus:build']
      },
      jade: {
        files: ['app/views/**/*.jade'],
        tasks: ['jade']
      },
      fonts: {
        files: ['app/assets/fonts/**/*'],
        tasks: ['copy:fonts']
      },
      test: {
        files: ['test/js/**/*.js'],
        tasks: ['jshint', 'qunit', 'copy:test']
      }
      // images: {
      //   files: ['app/assets/images/**/*'],
      //   tasks: ['copy:images']
      // },
      // videos: {
      //   files: ['app/assets/videos/**/*'],
      //   tasks: 'copy:videos'
      // },
      // flash: {
      //   files: ['app/assets/flash/**/*'],
      //   tasks: 'copy:flash'
      // }
      // vendor: {
      //   files: ['app/assets/vendor/**/*.css'],
      //   tasks: 'copy:build'
      // }
    },
    clean: {
      build: ['public']
    }
  });

  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-stylus');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-jade');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-cssmin');

  grunt.loadNpmTasks('grunt-usemin');


  grunt.registerTask('default', ['jshint', 'clean:build', 'concat', 'less:build', 'jade', 'copy']);
  grunt.registerTask('build', 'default');
  grunt.registerTask('release', ['build', 'uglify', 'cssmin', 'usemin:html']);
};
