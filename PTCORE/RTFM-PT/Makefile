
CFILES = RTFM-PT.c
AUTOGEN = ../Application/autogen.c
OUT = RTFM


Fll: $(CFILES) $(AUTOGEN) 
	gcc $(CFILES) -o $(OUT) -lpthread

trace: $(CFILES) $(AUTOGEN)
	gcc $(CFILES) -o $(OUT) -lpthread -D TRACE

clean:
	rm $(OUT) 
