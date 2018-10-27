include $(GNUSTEP_MAKEFILES)/common.make

APP_NAME = corekit-test

$(APP_NAME)_RESOURCE_FILES = \
	testimg.svg

$(APP_NAME)_OBJC_FILES = \
	main.m

$(APP_NAME)_INCLUDE_DIRS += `pkg-config --cflags clutter-0.9 librsvg-2.0`
$(APP_NAME)_GUI_LIBS += `pkg-config --libs clutter-0.9 librsvg-2.0` -lCoreKit

include $(GNUSTEP_MAKEFILES)/application.make

