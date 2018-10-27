# copyright 2003 Alexander Malmberg <alexander@malmberg.org>
#
# Standard GNUstep application makefile.

include $(GNUSTEP_MAKEFILES)/common.make

PACKAGE_NAME =	SVGViewer
CVS_MODULE_NAME = SVGViewer

CVS_OPTIONS = -d /opt/cvsroot
VERSION = 0.2.0


ADDITIONAL_OBJCFLAGS += -Wall `pkg-config --cflags libsvg`


APP_NAME = SVGViewer

SVGViewer_OBJC_FILES = \
	main.m \
	Document.m

SVGViewer_LDFLAGS += `pkg-config --libs libsvg`


BUNDLE_NAME = SVGImageRep

SVGImageRep_OBJC_FILES = \
	SVGImageRep.m

SVGImageRep_LDFLAGS += `pkg-config --libs libsvg`
SVGImageRep_PRINCIPAL_CLASS = SVGImageRepDelegate


include $(GNUSTEP_MAKEFILES)/application.make
include $(GNUSTEP_MAKEFILES)/bundle.make

