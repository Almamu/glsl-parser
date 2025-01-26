#define I_LIKE_TO 1
#define MOVE
#define MOVE_TO I_LIKE_TO + 1 // test

void testfunction() {
#define TEST2 MOVE_TO
    float value = TEST2;
}

#ifdef MOVE
float testvalue;
void test() {
}
#elif I_LIKE_TO == 0
void secondtest() {
    float third = testvalue;
}
#else
void test() {
    float asdf = 0;
}
#endif

#if I_LIKE_TO == 1
void secondtest() {
    float second = testvalue;
}
#elif I_LIKE_TO == 0
void secondtest() {
    float third = testvalue;
}
#endif

#ifndef I_LIKE_TO
void thirdtest() {

}
#endif

#ifdef UNDEFINED
void testing123() {}
#endif