#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main() {
    // PUSH_STRING "Hola"
    const char* str = "Hola";
    // STORE saludo (local)
    saludo = result;
    // PUSH_STRING "Mundo"
    const char* str = "Mundo";
    // STORE nombre (local)
    nombre = result;
    // PUSH_VAR saludo
    int saludo = 0;
    // PUSH_STRING " "
    const char* str = " ";
    // UNKNOWN
    // Unsupported operation
    // PUSH_VAR nombre
    int nombre = 0;
    // UNKNOWN
    // Unsupported operation
    // STORE mensaje (local)
    mensaje = result;
    // PUSH_VAR mensaje
    int mensaje = 0;
    // PRINT
    printf("%d\n", result);
    return 0;
}
