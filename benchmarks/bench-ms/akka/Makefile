default: build 


build_main:
	gradle -x test jarMain jarInstrumentationAgent 

build: protobuf  build_main

.PHONY:protobuf
protobuf:
	gradle wrapper
	./gradlew build -x test

.PHONY: test
test:
	gradle test