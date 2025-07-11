#include <malloc.h>
#include <string.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct _lambdummy_type {
    void *env;
    void *func;
} __lambdummy_type;
static __lambdummy_type *__lambdummy;

 __lambdummy_type *alloclabmd(int size, void *labmda, void *env)
{
    __lambdummy_type *ptr = malloc(sizeof(__lambdummy_type));
    ptr->env = malloc(size);
    memcpy(ptr->env, env, size);
    ptr->func = labmda;
    return ptr;
}

#define CALL_LAMBDA0(l, t) (__lambdummy = (__lambdummy_type *)l, ((t)(__lambdummy->func))())
#define CALL_LAMBDA(l, t, ...) (__lambdummy = (__lambdummy_type *)l,       ((t)(__lambdummy->func))(__VA_ARGS__))

typedef void (*l_int_to_void_r)(int);
typedef char* (*l_char_ptr__char_ptr__vararg_to_char_ptr_r)(char*, char*, ...);
typedef void (*l_BigInt__int_to_void_r)(void*, int);
typedef char* (*l_int__char_ptr__int_to_char_ptr_r)(int, char*, int);
typedef int (*l__to_int_r)();
typedef char* (*l_char_ptr__char_ptr_to_char_ptr_r)(char*, char*);
typedef struct {int _0; int _1; int _2; int _3; int _4; int _5;} tup_int__int__int__int__int__int_le;
typedef int (*l_int__int_to_int_r)(int, int);
typedef int (*l_char_ptr__vararg_to_int_r)(char*, ...);
typedef void* (*l_BigInt__BigInt_to_BigInt_r)(void*, void*);
typedef int (*l_BigInt__BigInt_to_int_r)(void*, void*);
typedef char* (*l_int_to_char_ptr_r)(int);
typedef char* (*l_BigInt_to_char_ptr_r)(void*);

struct BigInt {
    int* digits;
    l_BigInt__int_to_void_r constructor;
    l_BigInt__BigInt_to_BigInt_r add;
    l_BigInt__BigInt_to_BigInt_r multiply;
    l_BigInt__BigInt_to_BigInt_r subtract;
    l_BigInt__BigInt_to_BigInt_r divide;
    l_BigInt__BigInt_to_int_r compare;
    l_BigInt_to_char_ptr_r toString;
};
typedef struct BigInt* BigInt;


int main ();
static BigInt __perkelang_lambda_1 (BigInt, BigInt);
static char* __perkelang_lambda_6 (BigInt);
void leibniz_pi (int);
int mod (int, int);
static BigInt __perkelang_lambda_4 (BigInt, BigInt);
static BigInt __perkelang_lambda_3 (BigInt, BigInt);
static void __perkelang_lambda_0 (BigInt, int);
static int __perkelang_lambda_5 (BigInt, BigInt);
static BigInt __perkelang_lambda_2 (BigInt, BigInt);
int ipow (int, int);
char* string_of_digit (int);

int ipow(int base, int exp) {
    int result = 1;
    while (1) {

        if (exp & 1)
            result *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
        
    }
    return result;
}

char* string_of_digit(int a) {
    char c = ((char)(a + ((int)('0'))));
    char* out = "0";
    out[0] = c;
    return out;
}

int mod(int a, int b) {
    int x = 0;

    x = a % b;
    
    return x;
}

BigInt BigInt_init(int arg_0) {
    BigInt self = malloc(sizeof(struct BigInt));
    self->digits = (int*) ((int*)(0));
    self->constructor = (l_BigInt__int_to_void_r) __perkelang_lambda_0;
    self->add = (l_BigInt__BigInt_to_BigInt_r) __perkelang_lambda_1;
    self->multiply = (l_BigInt__BigInt_to_BigInt_r) __perkelang_lambda_2;
    self->subtract = (l_BigInt__BigInt_to_BigInt_r) __perkelang_lambda_3;
    self->divide = (l_BigInt__BigInt_to_BigInt_r) __perkelang_lambda_4;
    self->compare = (l_BigInt__BigInt_to_int_r) __perkelang_lambda_5;
    self->toString = (l_BigInt_to_char_ptr_r) __perkelang_lambda_6;

    self->constructor(self, arg_0);
    return self;
}

void leibniz_pi(int n) {
    tup_int__int__int__int__int__int_le t = (tup_int__int__int__int__int__int_le){1, 0, 1, 1, 3, 3};
    while (n > 0) {
        if (4 * t._0 + t._1 - t._2 < t._4 * t._2) {
            printf("%d", t._4);
            t = (tup_int__int__int__int__int__int_le){10 * t._0, 10 * (t._1 - t._4 * t._2), t._2, t._3, (10 * (3 * t._0 + t._1)) / t._2 - 10 * t._4, t._5};
            n--;
        } else {
            t = (tup_int__int__int__int__int__int_le){t._0 * t._3, (2 * t._0 + t._1) * t._5, t._2 * t._5, t._3 + 1, (t._0 * (7 * t._3 + 2) + t._1 * t._5) / (t._2 * t._5), t._5 + 2};
        }
    }
}

int main() {
    printf("Digits of pi:\n");
    BigInt a = BigInt_init(500);
    BigInt b = BigInt_init(500);
    BigInt c = a->multiply(a, b);
    printf("%s", c->toString(c));
    printf("\n");
    return 0;
}

static char* __perkelang_lambda_6(BigInt self) {
    char* result = malloc(51);
    int j = 0;
    int leading_zero = 1;
    for (int i = 49; i >= 0; i--) {
        printf("Digit %d: %d\n", i, self->digits[i]);
        if (self->digits[i] != 0 || !leading_zero) {
            leading_zero = 0;
            result[j] = ((char)(self->digits[i] + ((int)('0'))));
            j++;
        } else {

        }
    }
    return result;
}

static BigInt __perkelang_lambda_2(BigInt self, BigInt other) {
    BigInt result = BigInt_init(0);
    for (int i = 0; i < 50; i++) {
        for (int j = 0; j < 50; j++) {
            if (i + j < 50) {
                result->digits[i + j] = result->digits[i + j] + self->digits[i] * other->digits[j];
            } else {

            }
        }
    }
    return result;
}

static BigInt __perkelang_lambda_1(BigInt self, BigInt other) {
    BigInt result = BigInt_init(0);
    int carry = 0;
    for (int i = 0; i < 50; i++) {
        int sum = self->digits[i] + other->digits[i] + carry;
        result->digits[i] = mod(sum, 10);
        carry = sum / 10;
    }
    return result;
}

static BigInt __perkelang_lambda_4(BigInt self, BigInt other) {
    BigInt quotient = BigInt_init(0);
    BigInt remainder = BigInt_init(0);
    for (int i = 49; i >= 0; i--) {
        remainder->digits[i] = self->digits[i];
        for (int j = 0; j < 10; j++) {
            if (remainder->compare(remainder, other) >= 0) {
                remainder = remainder->subtract(remainder, other);
                quotient->digits[i] = quotient->digits[i] + 1;
            } else {
                break;
            }
        }
    }
    return quotient;
}

static int __perkelang_lambda_5(BigInt self, BigInt other) {
    for (int i = 49; i >= 0; i--) {
        if (self->digits[i] > other->digits[i]) {
            return 1;
        } else {
            if (self->digits[i] < other->digits[i]) {
                return -1;
            } else {

            }
        }
    }
    return 0;
}

static void __perkelang_lambda_0(BigInt self, int i) {
    self->digits = ((int*)(malloc(50 * 4)));
    for (int j = 0; j < 50; j++) {
        self->digits[j] = mod(i, 10);
        i = i / 10;
        printf("%d\n", self->digits[j]);
    }
}

static BigInt __perkelang_lambda_3(BigInt self, BigInt other) {
    BigInt result = BigInt_init(0);
    int borrow = 0;
    for (int i = 0; i < 50; i++) {
        int diff = self->digits[i] - other->digits[i] - borrow;
        if (diff < 0) {
            diff = diff + 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        result->digits[i] = diff;
    }
    return result;
}
