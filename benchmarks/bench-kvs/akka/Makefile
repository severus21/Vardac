default: build 


build_main:
	gradle -x test jarMain jarInstrumentationAgent 

build_lmain:
	gradle -x test jarLMain jarInstrumentationAgent 

build_clientService:
	gradle -x test jarGetRequestService jarInstrumentationAgent

build_shardService:
	gradle -x test jarGetResultService jarInstrumentationAgent

build_gRPCServer:
	gradle -x test jarGRPCServer  jarInstrumentationAgent

build_console:
	gradle -x test jarConsole  jarInstrumentationAgent

build: protobuf  build_main build_lmain build_clientService build_shardService build_gRPCServer build_console

.PHONY:protobuf
protobuf:
	gradle wrapper
	./gradlew build -x test

.PHONY: test
test:
	gradle test