include $(GNUSTEP_MAKEFILES)/common.make

LIBRARY_NAME = CoreKit
${LIBRARY_NAME}_VERSION = 0.1.0

$(LIBRARY_NAME)_OBJC_FILES = \
	GraphicsContext/GraphicsContext.m \
	CKSvg.m \
	CKWidget.m \
	CKWindow.m \
	CKSvgImage.m \
	CoreKit.m

$(LIBRARY_NAME)_HEADER_FILES = \
	GraphicsContext/GraphicsContextProtocol.h \
	GraphicsContext/GraphicsContext.h \
	CKSvg.h \
	CKWidget.h \
	CKWindow.h \
	CKSvgImage.h \
	CoreKit.h

$(LIBRARY_NAME)_HEADER_FILES_INSTALL_DIRS = CoreKit

${LIBRARY_NAME}_INCLUDE_DIRS += `pkg-config --cflags \
	clutter-0.9 \
	librsvg-2.0`
${LIBRARY_NAME}_LIBRARY_LIBS += `pkg-config --libs \
	clutter-0.9 \
	librsvg-2.0`

include $(GNUSTEP_MAKEFILES)/library.make

