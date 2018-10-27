include $(GNUSTEP_MAKEFILES)/common.make

OBJCFLAGS += -std=c99
CFLAGS += -std=c99

APP_NAME = OpenGrafik
# $(APP_NAME)_APPLICATION_ICON = 
$(APP_NAME)_MAIN_MODEL_FILE = MainUI.gorm

# $(APP_NAME)_RESOURCE_FILES = \
#	MainUI.gorm

$(APP_NAME)_LOCALIZED_RESOURCE_FILES = \
	MainUI.gorm \
	Document.gorm 

$(APP_NAME)_LANGUAGES = \
	English

$(APP_NAME)_OBJC_FILES = $(wildcard *.m)
$(APP_NAME)_HEADER_FILES = $(wildcard *.h)

include $(GNUSTEP_MAKEFILES)/application.make

