var fs = require('fs');

var debug = false;
var fileobj = null;

function showURL (pkgname, pkglist) {
    if (pkglist[pkgname]) {
	if (debug === true) console.log('INFO: ' + pkgname + ' exists');

	if (pkglist[pkgname]['dist-tags']) {
	    if (debug) console.log('INFO: ' + pkgname + '.dist-tags exists');

	    if (pkglist[pkgname]['dist-tags'].latest) {
		if (debug) console.log('INFO: ' + pkgname + '.dist-tags.latest exists');

		console.log('http://registry.npmjs.org/' + pkgname + '/-/' + pkgname + '-' + pkglist[pkgname]['dist-tags'].latest + '.tgz');
	    }
	}
    }
}

process.argv.forEach(function (val, index, array) {
    if (val === '--help') {
	console.log('Usage: node dep-url.js <file> <pkg> [OPTIONS]');
	console.log('');
	console.log('OPTIONS');
	console.log('  --help   Show this help');
	console.log('  --debug  Print debug information');
	console.log('');
	console.log('Unrecognized OPTIONS will be silently ignored.');
	process.exit(0);
    } else if (val === '--debug') {
	debug = true;
    }
});

if (process.argv.length < 4) {
    console.error('ERROR: Too little arguments.');
    console.error('ERROR: See --help');
    process.exit(1);
}

if (debug === true) {
    console.log('INFO: Will show dependencies for ' + process.argv[3]);
    console.log('INFO: Parsing file ' + process.argv[2]);
}

fileobj = JSON.parse(fs.readFileSync(process.argv[2]));
showURL(process.argv[3], fileobj);
