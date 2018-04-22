static/libasync-process.so: async-process.c
	gcc -Wextra -Wall -shared -fPIC -o static/libasync-process.so async-process.c
