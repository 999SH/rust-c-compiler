#include <stdio.h>

// A function declaration with parameters
int add(int a, int b) {
    // Binary operation
    int result = a + b;
    // Return statement
    return result;
}

int main() {
    // Variable declaration with initialization
    int x = 10;
    // Variable declaration without initialization
    int y;
    y = 20;

    // A function call
    int z = add(x, y);

    // Print the result
    printf("The sum is: %d\n", z);

    // Negation, BitNOT, Dereference, Address operations
    int negation = -x;
    unsigned int bit_not = ~x;
    int *ptr = &x;
    int deref = *ptr;

    printf("Negation of x: %d\n", negation);
    printf("BitNOT of x: %d\n", bit_not);
    printf("Address of x: %p\n", (void*)ptr);
    printf("Dereference of ptr: %d\n", deref);

    return 0;
}