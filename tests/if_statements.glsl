void main() {
    float value1 = 10;
    float value2 = 12;
    float value3 = 13;

    if (value1 == value2 && value2 == value1) {
        value1 = 5;
    } else {
        value1 = 2;
    }

    if (value1 == value2) {
        value2 = 5;
    } else if (value1 == value3) {
        value3 = 10;
    } else {
        value1 = 0;
    }
}