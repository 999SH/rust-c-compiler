int adder(int a, int b){
    int c = a + b;
    c = c + 5;
    return c + 5;
}

int main() {
    int d = adder(3,4);
    return 0;
}