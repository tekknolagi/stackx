int myfunction(int a, int b) {
    int sum = a + b;
    int sum2 = sum + sum;
    return sum;
}

int main() {
    int a = myfunction(2, 3);
    if (a == 0) {
        return 0;
    }
    else {
        return 1;
    }
}
