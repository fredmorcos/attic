import tango.io.Stdout,
	   tango.text.xml.Document,
	   tango.text.xml.DocPrinter,
	   tango.io.File;

int main () {
//	char[] x = import("hello.txt");
//	Stdout.formatln("{}", x);

	auto doc = new Document!(char);
	doc.header;
	doc.root.element(null, "vertex").attribute(null, "posx", "123").attribute(null, "posy", "345").element(null, "childvertex", "i'm a child!");

	auto print = new DocPrinter!(char);

	Stdout("Before writing to file!!! From XML tree!!!").newline;
	Stdout(print(doc)).newline;

	File myfile = new File("hello.xml");
	myfile.write(print(doc));

	char[] filetext = cast(char[]) myfile.read;
	Stdout("After reading from file!!! From String...").newline;
	Stdout(filetext).newline;

	Stdout("After parsing XML from char[]!!!").newline;
	doc.parse(filetext);
	Stdout(print(doc)).newline;

	Stdout("Going through the tree").newline;

	foreach(node; doc.query.child("vertex")) {
		foreach(attr; node.attributes()) {
			Stdout.formatln("{}: {}", attr.name, attr.value);
		}
	}

	delete myfile;
	delete print;
	delete doc;
	return 0;
}
