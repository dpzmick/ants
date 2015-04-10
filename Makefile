all:
	erl -make

run:
	erl -pa ebin/ +P 500000
