/* jslint strict: true */
/* jslint node: true */
/* jshint globalstrict: true */

"use strict";

var log_verbose = function(msg) {};

function ArgParseException(section, message) {
  this.section = section;
  this.message = message;

  this.toString = function() {
    return "" +
      "[ERROR] Cannot parse args in section: " + this.section + "\n" +
      "[ERROR] " + this.message + "\n" +
      "[INFO]  See `pet help` for more information";
  };
}

function command_help() {
  // TODO
  console.log("HELP!");
}

function command_version() {
  // TODO
  console.log("VERSION!");
}

function parse_global_args(args, files) {
  var arg = args.shift();
  var param;

  if (typeof arg === typeof undefined) {
    return;
  } else {
    if (arg === "-v") {
      log_verbose = function(msg) { console.log(msg); };
    } else if (arg === "-f") {
      param = args.shift();

      if (typeof param === typeof undefined) {
        throw new ArgParseException("Global", "Missing parameter for: " + arg);
      } else {
        files.push(param);
      }
    } else {
      args.unshift(arg);
      return;
    }
  }

  parse_global_args(args, files);
}

function parse_display_args(args, opts) {
  var arg = args.shift();

  if (typeof arg === typeof undefined) {
    return;
  } else {
    if (arg === "-T") {
      opts.tabulate = false;
    } else {
      args.unshift(arg);
      return;
    }
  }

  parse_display_args(args, opts);
}

function parse_add_args(args, opts) {
  var arg = args.shift();
  var param;
  var val;

  if (typeof arg === typeof undefined) {
    return;
  } else {
    if (arg === "-a") {         // amount
      param = args.shift();

      if (typeof param === typeof undefined) {
        throw new ArgParseException("Command <add>",
                                    "Missing parameter for: " + arg);
      } else {
        val = parseFloat(param);

        if (isNaN(val) === true) {
          throw new ArgParseException("Command <add>",
                                      "Invalid <amount> parameter: " + param);
        } else {
          opts.amount = val;
        }
      }
    } else if (arg === "-d") {  // date
      param = args.shift();

      if (typeof param === typeof undefined) {
        throw new ArgParseException("Command <add>",
                                    "Missing parameter for: " + arg);
      } else {
        val = parseFloat(param);

        if (isNaN(val) === true) {
          throw new ArgParseException("Command <add>",
                                      "Invalid <amount> parameter: " + param);
        } else {
          opts.amount = val;
        }
      }
    } else {
      args.unshift(arg);
      return;
    }
  }

  parse_display_args(args, opts);
}

function make_display_command(_opts) {
  return function(expenses) {
    var opts = _opts;
    // TODO
    return expenses;
  };
}

function parse_command_args(args, cmds) {
  var arg = args.shift();

  if (typeof arg === typeof undefined) {
    return;
  } else {
    if (arg === "help") {
      command_help();
      process.exit();
    } else if (arg === "version") {
      command_version();
      process.exit();
    } else if (arg === "add") {
    } else if (arg === "display") {
      var display_opts = {
        tabulate: true
      };

      parse_display_args(args, display_opts);
      cmds.push(make_display_command(display_opts));
    } else {
      throw new ArgParseException("Commands", "Unrecognized command: " + arg);
    }
  }

  parse_command_args(args, cmds);
}

function main () {
  var args = process.argv;
  args.shift();
  args.shift();

  var files = [];
  var command_list = [];

  try {
    parse_global_args(args, files);
    parse_command_args(args, command_list);
  } catch (e) {
    if (e instanceof ArgParseException) {
      console.error(e.toString());
      process.exit(1);
    } else {
      throw e;
    }
  }

  console.log(files);
  console.log(command_list);
}

main();
