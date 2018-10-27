include $(GNUSTEP_MAKEFILES)/common.make

APP_NAME = OpenGrafik

# $(APP_NAME)_APPLICATION_ICON = 

$(APP_NAME)_RESOURCE_FILES = $(wildcard Resources/*.gsmarkup)

$(APP_NAME)_LOCALIZED_RESOURCE_FILES = \
	DocumentWindow.strings \
	Menu-GNUstep.strings \
	Menu-OSX.strings

$(APP_NAME)_LANGUAGES = \
	French \
	Arabic

$(APP_NAME)_OBJC_FILES = $(wildcard *.m)
$(APP_NAME)_HEADER_FILES = $(wildcard *.h)

ifeq ($(FOUNDATION_LIB), apple)
	ADDITIONAL_INCLUDE_DIRS += -framework Renaissance
	ADDITIONAL_GUI_LIBS += -framework Renaissance
else
	ADDITIONAL_GUI_LIBS += -lRenaissance
endif

include $(GNUSTEP_MAKEFILES)/application.make

