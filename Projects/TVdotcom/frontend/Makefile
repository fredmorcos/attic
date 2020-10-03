OUT=normalize.min.css skeleton.min.css codez.min.js index.min.html

all: $(OUT)

%.min.css: %.css
	python /usr/lib/python3.8/site-packages/rcssmin.py < $< > $@

codez.min.js: codez.js
	python /usr/lib/python3.8/site-packages/rjsmin.py < $< > $@

index.min.html: index.html
	htmlmin -c -s -k $< $@

clean:
	$(RM) $(OUT)
