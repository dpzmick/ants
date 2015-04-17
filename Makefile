all:
	erl -make

run: all
	erl -pa ebin/

bigrun: all
	erl -pa ebin/ +P 134217727

clean:
	rm ebin/*
