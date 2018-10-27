include $(GNUSTEP_MAKEFILES)/common.make

APP_NAME = OpenGrafik

# $(APP_NAME)_RESOURCE_FILES = \

# $(APP_NAME)_LOCALIZED_RESOURCE_FILES = \

# $(APP_NAME)_LANGUAGES = \

$(APP_NAME)_OBJC_FILES = \
	main.m

# $(APP_NAME)_HEADER_FILES = \

$(APP_NAME)_INCLUDE_DIRS += `pkg-config --cflags clutter-0.9 gtk+-2.0`
$(APP_NAME)_GUI_LIBS += `pkg-config --libs clutter-0.9 gtk+-2.0` -lFrontStep

include $(GNUSTEP_MAKEFILES)/application.make

