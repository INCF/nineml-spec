all: libninemlnrn

libninemlnrn:
	gcc -fPIC -c -o nineml.o nineml.cpp 
	gcc -shared -Wl,-soname,libninemlnrn.so.1 -o libninemlnrn.so.1.0.1 nineml.o -lc
	ln -s libninemlnrn.so.1.0.1 libninemlnrn.so
	ln -s libninemlnrn.so.1.0.1 libninemlnrn.so.1

clean:
	rm -f libninemlnrn.so.1.0.1 libninemlnrn.so libninemlnrn.so.1 nineml.o

