.PHONY: all clean

TARGET = electrod

all:
	jbuilder build && ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

clean:
	@jbuilder clean
	@git clean -dfXq
	@rm -f ./$(TARGET)
