include $(GNUSTEP_MAKEFILES)/common.make

OBJCFLAGS += -std=c99
CFLAGS += -std=c99

APP_NAME = OpenGrafik
# $(APP_NAME)_APPLICATION_ICON = Images/OpenGrafik.tiff
$(APP_NAME)_MAIN_MODEL_FILE = \
	Resources/OpenGrafik.gorm

$(APP_NAME)_RESOURCES = \
	OpenGrafikInfo.plist

$(APP_NAME)_RESOURCE_FILES = \
	Resources/OpenGrafik.gorm \
	Resources/Document.gorm \
	Images/OpenGrafik.tiff \
	Images/OpenGrafik.eps

$(APP_NAME)_LOCALIZED_RESOURCE_FILES = \
	OpenGrafik.strings \
	Document.strings

$(APP_NAME)_LANGUAGES = \
	English \
	French

$(APP_NAME)_OBJC_FILES = $(wildcard *.m)
$(APP_NAME)_HEADER_FILES = $(wildcard *.h)

include $(GNUSTEP_MAKEFILES)/application.make

