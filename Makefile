default:
	jbuilder build @install

install:
	jbuilder install

testAll:
	jbuilder build @install
	jbuilder install
	jbuilder build test/test.exe
	./_build/default/test/test.exe