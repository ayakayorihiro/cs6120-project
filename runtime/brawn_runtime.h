#include <string>
#include <unordered_map>
#include <regex>

struct brawn_value_t;

typedef std::unordered_map<std::string, brawn_value_t*> brawn_array_t;

enum brawn_type_t {
    NUMBER,
    STRING,
    REGEX,
    ARRAY
};

struct brawn_value_t {
    brawn_type_t tag;
    union {
        brawn_array_t array_val;
        std::string   string_val;
        std::regex    regex_val;
        float         float_val;
    };
};

bool brawn_is_true(brawn_value_t*);
brawn_value_t* brawn_init();
brawn_value_t* brawn_from_number(float num);
brawn_value_t* brawn_from_string(std::string);
brawn_value_t* brawn_from_regex(std::regex);
brawn_value_t* brawn_array_assign(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_array_get(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_not(brawn_value_t*);
brawn_value_t* brawn_neg(brawn_value_t*);
brawn_value_t* brawn_add(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_sub(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_mult(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_div(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_pow(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_mod(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_and(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_or(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_lt(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_gt(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_le(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_ge(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_eq(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_ne(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_concat(brawn_value_t*, brawn_value_t*);
brawn_value_t* brawn_match(brawn_value_t*, brawn_value_t*);
