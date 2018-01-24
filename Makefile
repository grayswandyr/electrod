.PHONY: all clean

TARGET = electrod

all: build

build:
	jbuilder build @install && ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

install: build
	@jbuilder install

uninstall:
	@jbuilder uninstall

clean:
	@jbuilder clean
	@git clean -dfXq
	@rm -f ./$(TARGET)
