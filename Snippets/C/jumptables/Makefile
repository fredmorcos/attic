BINS = jumptable jumptable-labels
ASMS = $(BINS:%=%.s)

BUILD_CMD = $(CC) $(CFLAGS) -O0 -Wall -Wextra -std=c11

all: $(BINS) $(ASMS)

%: %.c
	$(BUILD_CMD) $< -o $@

%.s: %.c
	$(BUILD_CMD) $< -S

clean:
	rm -rf $(BINS) $(ASMS)
