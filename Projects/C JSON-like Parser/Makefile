CFLAGS += -D_DEFAULT_SOURCE
CFLAGS += -D_FILE_OFFSET_BITS=64
CFLAGS += -D_FORTIFY_SOURCE

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -W
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -Wpedantic

ifeq ($(release),yes)
CFLAGS += -DNDEBUG -O2
else
CFLAGS += -DDEBUG -O -g
endif

.PHONY: all clean install cppcheck

all: libjson.so libjson.a json-test

json.o: json.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -fPIC -c $< -o $@

json-test.o: CFLAGS += -DLIBJSON_TEST
json-test.o: json.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

json-test: json-test.o
	$(CC) $(LDFLAGS) $^ $(LOADLIBES) $(LDLIBS) -o $@

libjson.so: json.o
	$(CC) $(LDFLAGS) $^ $(LOADLIBES) $(LDLIBS) -shared -fPIC -o $@

libjson.a: json.o
	ar rcs $@ $^

clean:
	$(RM) json.o libjson.so libjson.a json-test.o json-test

.ONESHELL:
install: all
ifeq ($(DEST),)
	$(warning DEST is not set, will assume DEST=/usr/local)
	$(eval DEST = /usr/local)
endif
	$(eval SHARE_DIR = $(DEST)/share/libjson)
	$(eval LIB_DIR   = $(DEST)/lib)
	mkdir -p $(SHARE_DIR)
	mkdir -p $(LIB_DIR)
	install -m 644 LICENSE $(SHARE_DIR)
	install -m 755 libjson.so $(LIB_DIR)
	install -m 644 libjson.a $(LIB_DIR)

CPPC_FLAGS += --std=c99 --std=posix --enable=all
CPPC_FLAGS += --suppress=missingIncludeSystem
CPPC_FLAGS += --suppress=readdirCalled
CPPC_FLAGS += --suppress=unmatchedSuppression

cppcheck: json.c
	cppcheck $(CPPC_FLAGS) $^
