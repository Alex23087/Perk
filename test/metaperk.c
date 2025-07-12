#include <malloc.h>
#include <string.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
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

typedef void (*l_char_ptr_to_void_r)(char*);
typedef char* (*l_char_ptr_to_char_ptr_r)(char*);
typedef void* (*l_TokenStack_to_TokenStack_r)(void*);
typedef struct char_ptr_opt {int is_some; char* contents;} char_ptr_opt;
typedef struct {char* _0; int _1;} tup_char_ptr__int_le;
typedef struct {void* _0; int _1;} tup_void_ptr__int_le;
typedef void* (*l_void_ptr_to_void_ptr_r)(void*);
typedef struct TokenStack_opt {int is_some; void* contents;} TokenStack_opt;
typedef void (*l_TokenStack_to_void_r)(void*);
typedef int int_256_arr[256];
typedef char* (*l_char_ptr__int__int_to_char_ptr_r)(char*, int, int);
typedef int (*l__to_int_r)();
typedef bool (*l_char_ptr_to_bool_r)(char*);
typedef int (*l_char_to_int_r)(char);
typedef char (*l_int_to_char_r)(int);
typedef char_ptr_opt (*l_size_t_to_char_ptr_opt_r)(size_t);
typedef void (*l_TokenStack__tup_char_ptr__int_le_to_void_r)(void*, tup_char_ptr__int_le);
typedef tup_char_ptr__int_le (*l_TokenStack_to_tup_char_ptr__int_le_r)(void*);
typedef tup_char_ptr__int_le (*l_char_ptr_to_tup_char_ptr__int_le_r)(char*);
typedef tup_void_ptr__int_le (*l_void_ptr_to_tup_void_ptr__int_le_r)(void*);
typedef struct env_ {;} env_;
struct l_char_ptr_to_bool_r_env {
    void* env;
    l_char_ptr_to_bool_r func;
};
typedef struct l_char_ptr_to_bool_r_env* l_char_ptr_to_bool_r_env;
typedef struct env_char_ptr {char* _0;} env_char_ptr;

struct l_int_to_char_r_env {
    void* env;
    l_int_to_char_r func;
};
typedef struct l_int_to_char_r_env* l_int_to_char_r_env;
typedef struct env_size_t_char_ptr {size_t _0; char* _1;} env_size_t_char_ptr;
typedef l_char_ptr_to_bool_r_env (*l_char_ptr_to_l_char_ptr_to_bool_r_env_r)(char*);
typedef l_int_to_char_r_env (*l_char_ptr_to_l_int_to_char_r_env_r)(char*);
typedef void* (*l_char_ptr_to_void_ptr_r)(char*);

struct TokenStack {
    tup_char_ptr__int_le val;
    TokenStack_opt next;
    l_TokenStack__tup_char_ptr__int_le_to_void_r constructor;
    l_TokenStack__tup_char_ptr__int_le_to_void_r push;
    l_TokenStack_to_tup_char_ptr__int_le_r pop;
    l_TokenStack_to_TokenStack_r copy;
    l_TokenStack_to_void_r print;
};
typedef struct TokenStack* TokenStack;


int main ();
static void __perkelang_lambda_1 (TokenStack, tup_char_ptr__int_le);
static bool __perkelang_lambda_6 (char*);
tup_char_ptr__int_le parse_int (char*);
void* skip_computation (void*);
char_ptr_opt alloc_string (size_t);
char* reverse_string (char*);
tup_char_ptr__int_le lex_operator (char*);
static char __perkelang_lambda_7 (int);
static TokenStack __perkelang_lambda_3 (TokenStack);
char* substring (char*, int, int);
l_char_ptr_to_bool_r_env streq (char*);
static void __perkelang_lambda_0 (TokenStack, tup_char_ptr__int_le);
static void __perkelang_lambda_5 (TokenStack);
static tup_char_ptr__int_le __perkelang_lambda_2 (TokenStack);
l_int_to_char_r_env str_get_index (char*);
void failwith (char*);
tup_void_ptr__int_le execute_computation (void*);
void* tokenize_string (char*);

TokenStack TokenStack_init(tup_char_ptr__int_le arg_0) {
    TokenStack self = malloc(sizeof(struct TokenStack));
    
self->val = (tup_char_ptr__int_le) (tup_char_ptr__int_le){"END", 0};
    
self->next = (TokenStack_opt) ((TokenStack_opt) {0, 0});
    self->constructor = (l_TokenStack__tup_char_ptr__int_le_to_void_r) __perkelang_lambda_0;
    self->push = (l_TokenStack__tup_char_ptr__int_le_to_void_r) __perkelang_lambda_1;
    self->pop = (l_TokenStack_to_tup_char_ptr__int_le_r) __perkelang_lambda_2;
    self->copy = (l_TokenStack_to_TokenStack_r) __perkelang_lambda_3;
    self->print = (l_TokenStack_to_void_r) __perkelang_lambda_5;

    self->constructor(self, arg_0);
    return self;
}

void failwith(char* msg) {
    printf("Failure: %s\n", msg);
    exit(1);
}

char_ptr_opt alloc_string(size_t size) {
    char* sub = ((char*)(malloc(((size_t)(size)) + ((size_t)(1)))));
    int success = 0;

    success = (sub != 0);
    
    if (success) {
        return ((char_ptr_opt) {1, sub});
    } else {
        return ((char_ptr_opt) {0, 0});
    }
}

l_char_ptr_to_bool_r_env streq(char* s1) {
    return ((l_char_ptr_to_bool_r_env) alloclabmd(sizeof(env_char_ptr), __perkelang_lambda_6, (void*)&((env_char_ptr) {s1})));
}

l_int_to_char_r_env str_get_index(char* s) {
    size_t len = strlen(s);
    return ((l_int_to_char_r_env) alloclabmd(sizeof(env_size_t_char_ptr), __perkelang_lambda_7, (void*)&((env_size_t_char_ptr) {len, s})));
}

char* substring(char* str, int start, int end) {
    size_t len = strlen(str);
    if (start > end || start > len || end > len) {
        failwith("Invalid substring range");
    } else {

    }
    int sublen = ((int)(end)) - ((int)(start));
    char_ptr_opt sub = alloc_string(((size_t)(sublen)));
    if (!sub.is_some) {
        failwith("Malloc Failed");
    } else {

    }
    char* sub_ptr = ((char*)sub.contents);

    memcpy(sub_ptr, str + start, sublen);
    sub_ptr[sublen] = '\0';
    
    return sub_ptr;
}

tup_char_ptr__int_le lex_operator(char* s) {
    l_char_ptr_to_bool_r_env eq = streq(s);
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "+")) {
        return (tup_char_ptr__int_le){"Plus", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "*")) {
        return (tup_char_ptr__int_le){"Times", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "/")) {
        return (tup_char_ptr__int_le){"Div", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "-")) {
        return (tup_char_ptr__int_le){"Minus", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "=")) {
        return (tup_char_ptr__int_le){"Eq", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "<")) {
        return (tup_char_ptr__int_le){"Lt", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "!=")) {
        return (tup_char_ptr__int_le){"Neq", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "choose")) {
        return (tup_char_ptr__int_le){"Choose", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, ";")) {
        return (tup_char_ptr__int_le){"Seq", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "store")) {
        return (tup_char_ptr__int_le){"Store", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "load")) {
        return (tup_char_ptr__int_le){"Load", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "print")) {
        return (tup_char_ptr__int_le){"Print", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "repeat")) {
        return (tup_char_ptr__int_le){"Repeat", 0};
    } else {

    }
    if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "while")) {
        return (tup_char_ptr__int_le){"While", 0};
    } else {

    }
    failwith("Unrecognized arithmetical operator");
}

tup_char_ptr__int_le parse_int(char* s) {
    return (tup_char_ptr__int_le){"Num", atoi(s)};
}

char* reverse_string(char* s) {
    size_t len = strlen(s);
    char* r = ((char*)(malloc(((size_t)(len)) + ((size_t)(1)))));

    if (!r) return NULL;
    for (size_t i = 0; i < len; ++i) {
        r[i] = s[len - 1 - i];
    }
    r[len] = '\0';
    
    return r;
}

void* tokenize_string(char* s) {
    s = reverse_string(s);
    TokenStack stack = TokenStack_init((tup_char_ptr__int_le){"END", 0});
    size_t len = strlen(s);
    l_int_to_char_r_env get = str_get_index(s);
    int cur_word_start = 0;
    int cur_word_end = 0;
    int i = 0;
    for (i = 0; i < ((size_t)(len)) + ((size_t)(1)); i++) {
        char c = (i >= len ? ' ' : CALL_LAMBDA(get, l_int_to_char_r, i));
        if ((isspace(c)) && cur_word_start != cur_word_end) {
            char* sub_1 = substring(s, cur_word_start, cur_word_end);
            char* sub = reverse_string(sub_1);
            tup_char_ptr__int_le tok = (tup_char_ptr__int_le){"", 0};
            if (isdigit(CALL_LAMBDA(get, l_int_to_char_r, cur_word_start))) {
                tok = parse_int(sub);
            } else {
                tok = lex_operator(sub);
            }
            TokenStack __perkelang_model_appl_8 = stack;
            __perkelang_model_appl_8 -> push (__perkelang_model_appl_8, tok);
            free(((void*)(sub)));
            free(((void*)(sub_1)));
        } else {

        }
        if (isspace(c)) {
            cur_word_end = ((int)(i)) + ((int)(1));
            cur_word_start = cur_word_end;
        } else {
            cur_word_end = ((int)(i)) + ((int)(1));
        }
    }
    free(((void*)(s)));
    return stack;
}

int_256_arr memory = {};

void* skip_computation(void* stack_vp) {
    TokenStack stack = ((TokenStack)(stack_vp));
    l_char_ptr_to_bool_r_env done = streq("END");
    TokenStack __perkelang_model_appl_9 = stack;
    tup_char_ptr__int_le tok = __perkelang_model_appl_9 -> pop (__perkelang_model_appl_9);
    if (!CALL_LAMBDA(done, l_char_ptr_to_bool_r, tok._0)) {
        l_char_ptr_to_bool_r_env eq = streq(tok._0);
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Num")) {
            return (((void*)(stack)));
        } else {
            char* op = tok._0;
        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Plus") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Times") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Div") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Minus") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Eq") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Lt") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Neq") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Seq") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Store") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Repeat") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "While")) {
            void* res1 = skip_computation(((void*)(stack)));
            void* res2 = skip_computation(res1);
            return ((void*)(stack));
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Choose")) {
            void* res1 = skip_computation(((void*)(stack)));
            void* res2 = skip_computation(res1);
            void* res3 = skip_computation(res2);
            return ((void*)(stack));
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Load") || CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Print")) {
            void* res1 = skip_computation(((void*)(stack)));
            return ((void*)(stack));
        } else {

        }
        failwith("Operation not yet implemented");
    } else {

    }
    failwith("Skip reached end of stack.");
}

tup_void_ptr__int_le execute_computation(void* stack_vp) {
    TokenStack stack = ((TokenStack)(stack_vp));
    l_char_ptr_to_bool_r_env done = streq("END");
    TokenStack __perkelang_model_appl_10 = stack;
    tup_char_ptr__int_le tok = __perkelang_model_appl_10 -> pop (__perkelang_model_appl_10);
    if (!CALL_LAMBDA(done, l_char_ptr_to_bool_r, tok._0)) {
        l_char_ptr_to_bool_r_env eq = streq(tok._0);
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Num")) {
            return (tup_void_ptr__int_le){((void*)(stack)), tok._1};
        } else {
            char* op = tok._0;
        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Plus")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1)) + ((int)(res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Times")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1)) * ((int)(res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Div")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            if (res2._1 == 0) {
                failwith("Attempted division by zero!");
            } else {

            }
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1)) / ((int)(res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Eq")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1 == res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Lt")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1 < res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Neq")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1 != res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Minus")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), ((int)(res1._1)) - ((int)(res2._1))};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Choose")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = (res1._1 == 1 ? execute_computation(res1._0) : (tup_void_ptr__int_le){skip_computation(res1._0), 0});
            tup_void_ptr__int_le res3 = (res1._1 != 1 ? execute_computation(res2._0) : (tup_void_ptr__int_le){skip_computation(res2._0), 0});
            return (tup_void_ptr__int_le){((void*)(stack)), (res1._1 == 1 ? res2._1 : res3._1)};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Seq")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            return (tup_void_ptr__int_le){((void*)(stack)), res2._1};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Load")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            if (res1._1 < 0 || res1._1 >= 256) {
                failwith("metaperk: attempted out of bounds read");
            } else {

            }
            return (tup_void_ptr__int_le){((void*)(stack)), memory[res1._1]};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Store")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = execute_computation(res1._0);
            if (res1._1 < 0 || res1._1 >= 256) {
                failwith("metaperk: attempted out of bounds read");
            } else {

            }
            memory[res1._1] = res2._1;
            return (tup_void_ptr__int_le){((void*)(stack)), 0};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Print")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            printf("Program printed: %d\n", res1._1);
            return (tup_void_ptr__int_le){((void*)(stack)), 0};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "Repeat")) {
            tup_void_ptr__int_le res1 = execute_computation(((void*)(stack)));
            int iter = 0;
            tup_void_ptr__int_le res2 = (tup_void_ptr__int_le){((void*)(stack)), 0};
            for (iter = 0; iter < res1._1; iter++) {
                TokenStack __perkelang_model_appl_11 = ((TokenStack)(res1._0));
                TokenStack stack_copy_vp = __perkelang_model_appl_11 -> copy (__perkelang_model_appl_11);
                TokenStack stack_copy = ((TokenStack)(stack_copy_vp));
                res2 = execute_computation(stack_copy_vp);
            }
            return (tup_void_ptr__int_le){((void*)(stack)), res2._1};
        } else {

        }
        if (CALL_LAMBDA(eq, l_char_ptr_to_bool_r, "While")) {
            TokenStack __perkelang_model_appl_12 = stack;
            TokenStack cond_stack = __perkelang_model_appl_12 -> copy (__perkelang_model_appl_12);
            tup_void_ptr__int_le cond_eval = execute_computation(((void*)(stack)));
            tup_void_ptr__int_le res2 = (tup_void_ptr__int_le){((void*)(stack)), 0};
            while (cond_eval._1 == 1) {
                TokenStack __perkelang_model_appl_13 = ((TokenStack)(cond_eval._0));
                TokenStack body_stack = __perkelang_model_appl_13 -> copy (__perkelang_model_appl_13);
                res2 = execute_computation(body_stack);
                                TokenStack __perkelang_model_appl_14 = cond_stack;
                cond_eval = execute_computation(__perkelang_model_appl_14 -> copy (__perkelang_model_appl_14));
            }
            return (tup_void_ptr__int_le){((void*)(stack)), res2._1};
        } else {

        }
        failwith("Operation not yet implemented");
    } else {

    }
    failwith("Computation reached end of stack.");
}

int main() {
    char* fib = "; ; store 1 0 store 2 1 while < load 3 100 ; ; ; store 3 + load 1 load 2 print load 3 store 1 load 2 store 2 load 3";
    TokenStack toks = ((TokenStack)(tokenize_string(fib)));
    printf("Program tokens:\n");
    TokenStack __perkelang_model_appl_15 = toks;
    __perkelang_model_appl_15 -> print (__perkelang_model_appl_15);
    printf("\n==========\n\n");
    tup_void_ptr__int_le res = execute_computation(((void*)(toks)));
    printf("\nprogram result:\t %d\n", res._1);
    return 0;
}

static void __perkelang_lambda_1(TokenStack self, tup_char_ptr__int_le tok) {
    TokenStack new_elem = TokenStack_init(tok);
    TokenStack last_elem = self;
    while (last_elem->next.is_some) {
        last_elem = ((TokenStack)last_elem->next.contents);
    }
    last_elem->next = ((TokenStack_opt){1, new_elem});
}

static void __perkelang_lambda_5(TokenStack self) {
    TokenStack cur_elem = self;
    while (cur_elem->next.is_some) {
        tup_char_ptr__int_le val = cur_elem->val;
        printf("%s\t%d\n", val._0, val._1);
        cur_elem = ((TokenStack)cur_elem->next.contents);
    }
    tup_char_ptr__int_le val = cur_elem->val;
    printf("%s\t%d\n", val._0, val._1);
}

static void __perkelang_lambda_0(TokenStack self, tup_char_ptr__int_le tok) {
    self->val = tok;
}

static TokenStack __perkelang_lambda_3(TokenStack self) {
    TokenStack res = TokenStack_init(self->val);
    if (!self->next.is_some) {
        return res;
    } else {

    }
        TokenStack __perkelang_model_appl_4 = ((TokenStack)self->next.contents);
    res->next = ((TokenStack_opt){1, __perkelang_model_appl_4 -> copy (__perkelang_model_appl_4)});
    return res;
}

static char __perkelang_lambda_7(int i) {
    env_size_t_char_ptr* _env = (env_size_t_char_ptr*) __lambdummy->env;
    size_t len = (size_t)_env->_0;
    char* s = (char*)_env->_1;
    if (i >= len) {
        failwith("Tried to get out of bounds string index");
    } else {

    }
    char c = ((char)(0));
    char* _s = s;

        c = _s[i];
        
    return c;
}

static bool __perkelang_lambda_6(char* s2) {
    env_char_ptr* _env = (env_char_ptr*) __lambdummy->env;
    char* s1 = (char*)_env->_0;
    return (strcmp(s1, s2) == 0);
}

static tup_char_ptr__int_le __perkelang_lambda_2(TokenStack self) {
    TokenStack last_elem = self;
    TokenStack new_last_elem = self;
    while (last_elem->next.is_some) {
        new_last_elem = last_elem;
        last_elem = ((TokenStack)last_elem->next.contents);
    }
    new_last_elem->next = ((TokenStack_opt) {0, 0});
    tup_char_ptr__int_le res = last_elem->val;
    if (((int)(((void*)(last_elem)) != ((void*)(self))))) {
        free(last_elem);
        last_elem = NULL;
    } else {

    }
    return res;
}
