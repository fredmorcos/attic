include $(GNUSTEP_MAKEFILES)/common.make

LIBRARY_NAME = FrontStep

# $(LIBRARY_NAME)_RESOURCE_FILES = \

# $(LIBRARY_NAME)_LOCALIZED_RESOURCE_FILES = \

# $(LIBRARY_NAME)_LANGUAGES = \

$(LIBRARY_NAME)_OBJC_FILES = \
	NSXMLParserController.m \
	FSWindow.m \
	NSXMLParser+File.m \
	FrontStep.m

$(LIBRARY_NAME)_HEADER_FILES = \
	NSXMLParserController.h \
	FSWindow.h \
	NSXMLParser+File.h \
	FrontStep.h

$(LIBRARY_NAME)_HEADER_FILES_INSTALL_DIRS = FrontStep

${LIBRARY_NAME}_INCLUDE_DIRS += `pkg-config --cflags gtk+-2.0`
${LIBRARY_NAME}_LIBRARY_LIBS += `pkg-config --libs gtk+-2.0`

include $(GNUSTEP_MAKEFILES)/library.make

