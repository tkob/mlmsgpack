#include <msgpack.h>
#include <stdio.h>

#define BUFFER_SIZE (8 * 1024)

int main(int argc, char* argv[]) {
        msgpack_unpacker unpacker;
        msgpack_unpacked unpacked;
        char* buffer;
        size_t size;

        msgpack_unpacker_init(&unpacker, MSGPACK_UNPACKER_INIT_BUFFER_SIZE);
        msgpack_unpacked_init(&unpacked);

        while (!feof(stdin)) {
                /* ensure the unpacker to have enough buffer */
                msgpack_unpacker_reserve_buffer(&unpacker, BUFFER_SIZE);

                /* feed buffer of the unpacker from stdin */
                buffer = msgpack_unpacker_buffer(&unpacker);
                size = fread(buffer, 1, BUFFER_SIZE, stdin);
                if (size == 0) return 0;

                /* tell the unpacker the size of the data fed */
                msgpack_unpacker_buffer_consumed(&unpacker, size);

                /* get and print objects unpacked so far */
                while (msgpack_unpacker_next(&unpacker, &unpacked)) {
                        msgpack_object_print(stdout, unpacked.data);
                        printf("\n");
                }
        }
}
