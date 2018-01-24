.PHONY: all clean

TARGET = electrod

all: build

build:
	jbuilder build @install --dev \
	&& ln -sf _build/install/default/bin/$(TARGET) ./$(TARGET)

doc:
	BROWSER=x-www-browser topkg doc -r

install: build
	@jbuilder install

uninstall:
	@jbuilder uninstall

clean:
	@jbuilder clean
	@git clean -dfXq
	@rm -f ./$(TARGET)
