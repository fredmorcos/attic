include $(GNUSTEP_MAKEFILES)/common.make

LIBRARY_NAME = FrontStep

# $(LIBRARY_NAME)_RESOURCE_FILES = \

# $(LIBRARY_NAME)_LOCALIZED_RESOURCE_FILES = \

# $(LIBRARY_NAME)_LANGUAGES = \

$(LIBRARY_NAME)_OBJC_FILES = \
	FSWidget.m \
	FSWindow.m \
	FrontStep.m

$(LIBRARY_NAME)_HEADER_FILES = \
	FSWidget.h \
	FSWindow.h \
	FrontStep.h

$(LIBRARY_NAME)_HEADER_FILES_INSTALL_DIRS = FrontStep

${LIBRARY_NAME}_INCLUDE_DIRS += `pkg-config --cflags clutter-0.9 gtk+-2.0`
${LIBRARY_NAME}_LIBRARY_LIBS += `pkg-config --libs clutter-0.9 gtk+-2.0`

include $(GNUSTEP_MAKEFILES)/library.make

