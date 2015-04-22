PA=-pa ebin/ jiffy/ebin/

all:
	erl -make

run: all
	erl $(PA)

bigrun: all
	erl $(PA) +P 134217727

clean:
	rm ebin/*

test: all
	./test
