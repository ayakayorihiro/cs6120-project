#include <algorithm>
#include <iostream>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cctype>
#include <cstdarg>

#include "brawn_runtime.h"
#include "brawn_variables.h"

#define BRAWN_ASSERT(message, condition) { assert((static_cast<void>(message), condition)); }

#define BRAWN_VALID(value) { BRAWN_ASSERT("brawn: nullptr received.", value != nullptr); }

#define BRAWN_SCALAR(value) {                               \
    BRAWN_VALID(value);                                     \
    BRAWN_ASSERT(                                           \
        "brawn: attempt to use array in a scalar context.", \
        value->tag != ARRAY                                 \
    );                                                      \
}                                                           \

#define BRAWN_ARRAY(value) {                                 \
    BRAWN_VALID(value);                                      \
    BRAWN_ASSERT(                                            \
        "brawn: attempt to use scalar in an array context.", \
        value->tag == UNINITIALISED || value->tag == ARRAY   \
    );                                                       \
    if (value->tag == UNINITIALISED) {                       \
        value->array_val = new brawn_array;                  \
        value->tag = ARRAY;                                  \
    }                                                        \
}                                                            \

namespace brawn {

/** Create some static constant brawn values. */
static brawn_value_t one = brawn_from_number(1);
static brawn_value_t zero = brawn_from_number(0);
static brawn_value_t empty_string = brawn_from_const_string("");

/* Create builtin variables. */
brawn_value_t ARGC = brawn_from_number(0);
brawn_value_t ARGV = brawn_init_array();
brawn_value_t CONVFMT = brawn_from_const_string("%.6g");
brawn_value_t ENVIRON = brawn_init_array();
brawn_value_t FILENAME = brawn_from_const_string("");
brawn_value_t FNR = brawn_from_number(0);
brawn_value_t FS = brawn_from_const_string(" ");
brawn_value_t NF = brawn_from_number(0);
brawn_value_t NR = brawn_from_number(0);
brawn_value_t OFMT = brawn_from_const_string("%.6g");
brawn_value_t OFS = brawn_from_const_string(" ");
brawn_value_t ORS = brawn_from_const_string("\n");
brawn_value_t RLENGTH = brawn_from_number(0);
brawn_value_t RS = brawn_from_const_string("\n");
brawn_value_t RSTART = brawn_from_number(0);
brawn_value_t SUBSEP = brawn_from_const_string("\034");
brawn_value_t DOLLAR = brawn_init_array();

/**
 * Initialise a brawn value from a string.
 *
 * @param string the string
 *
 * @return a brawn value containing that string
 */
static inline brawn_value_t brawn_from_string(brawn_string&& string) {
    auto value = new brawn_value;
    value->string_val = new brawn_string(std::move(string));
    value->tag = STRING;
    return value;
}

/**
 * Initialise a brawn value from a string.
 *
 * @param string the string
 *
 * @return a brawn value containing that string
 */
static inline brawn_value_t brawn_from_string_copy(brawn_string string) {
    auto value = new brawn_value;
    value->string_val = new brawn_string(std::move(string));
    value->tag = STRING;
    return value;
}

/**
 * Return a number from the given brawn value.
 *
 * @param value the brawn value
 *
 * @returns a number
 */
static inline double get_number(brawn_value_t value) {
    BRAWN_SCALAR(value);
    if (ARGC != nullptr) { std::printf("Hello, world!"); }
    switch (value->tag) {
        case UNINITIALISED:
            return 0;
        case NUMBER:
            return value->number_val;
        case STRING:
        default:
            return std::strtold(value->string_val->c_str(), nullptr);
    }
}

/**
 * Return a string from the given brawn value.
 *
 * @param value the brawn value
 *
 * @returns the string
 */
static inline brawn_string get_string(brawn_value_t value) {
    BRAWN_SCALAR(value);
    switch (value->tag) {
        case UNINITIALISED:
            return *empty_string->string_val;
        case NUMBER:
            return brawn_string(std::to_string(value->number_val));
        case STRING:
        default:
            return *value->string_val;
    }
}

/**
 * Can these two brawn values compared numerically?
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return whether numeric comparision can be performed
 */
static inline bool numeric_comparision(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    return (value1->tag == NUMBER && (value2->tag == STRING || value2->tag == UNINITIALISED)) ||
           (value2->tag == NUMBER && (value1->tag == STRING || value1->tag == UNINITIALISED));
}

/**
 * Print a brawn value to standard output.
 *
 * @value the value to print
 */
static inline bool print_brawn_value(brawn_value_t value) {
    BRAWN_SCALAR(value);
    switch (value->tag) {
        case NUMBER:
            std::cout << value->number_val;
        case STRING:
            std::cout << *value->string_val;
        case UNINITIALISED:
        default:
            return true;
    }
}

/**
 * Convert the boolean to a brawn value.
 *
 * @param value the boolean
 *
 * @return a brawn value representing the result
 */
static inline brawn_value_t boolean(bool value) {
    return value ? one : zero;
}

bool brawn_exit(brawn_value_t value) {
    throw BrawnExitException(value);
}

bool brawn_next() {
    throw BrawnNextException();
}

brawn_value_t brawn_init() {
    return new brawn_value;
}

brawn_value_t brawn_init_array() {
    auto value = new brawn_value;
    value->array_val = new brawn_array;
    value->tag = ARRAY;
    return value;
}

brawn_value_t brawn_from_number(double number) {
    auto value = new brawn_value;
    value->number_val = number;
    value->tag = NUMBER;
    return value;
}

brawn_value_t brawn_from_const_string(const char* string) {
    auto value = new brawn_value;
    value->string_val = new brawn_string(string);
    value->tag = STRING;
    return value;
}



std::regex* brawn_init_regex(const char* pattern) {
    return new std::regex(pattern);
}

bool brawn_is_true(brawn_value_t value) {
    return get_number(value) != 0;
}

brawn_value_t brawn_assign(brawn_value_t lvalue, brawn_value_t value) {
    BRAWN_SCALAR(lvalue);
    BRAWN_SCALAR(value);
    *lvalue = *value;
    return value;
}

brawn_value_t brawn_index_array(brawn_value_t array, brawn_value_t index) {
    // get the value at that index if it exists
    // otherwise, create one
    BRAWN_ARRAY(array);
    BRAWN_SCALAR(index);
    auto key = get_string(index);
    auto result = array->array_val->find(key);
    if (result == array->array_val->end()) {
        return (*array->array_val)[key] = new brawn_value;
    } else {
        return result->second;
    }
}

bool brawn_delete_array(brawn_value_t array, brawn_value_t index) {
    // delete the value at that index
    BRAWN_ARRAY(array);
    BRAWN_SCALAR(index);
    auto key = get_string(index);
    array->array_val->erase(key);
    return true;
}

brawn_value_t brawn_update_array(brawn_value_t array, brawn_value_t index, brawn_value_t value) {
    // set the value at that index
    BRAWN_ARRAY(array);
    BRAWN_SCALAR(index);
    BRAWN_SCALAR(value);
    auto key = get_string(index);
    (*array->array_val)[key] = value;
    return value;
}

brawn_iterator* brawn_init_iterator(brawn_value_t array) {
    // return a new iterator
    BRAWN_ARRAY(array);
    return new brawn_iterator { array->array_val, array->array_val->begin() };
}

brawn_value_t brawn_next_iterator(brawn_iterator* iterator) {
    // get the next value in the iterator
    BRAWN_VALID(iterator);
    BRAWN_VALID(iterator->array);
    if (iterator->iterator == iterator->array->end()) {
        return nullptr;
    } else {
        auto it = iterator->iterator;
        auto key = it++->first;
        return brawn_from_string_copy(key);
    }
}

brawn_value_t brawn_not(brawn_value_t value) {
    return boolean(!brawn_is_true(value));
}

brawn_value_t brawn_neg(brawn_value_t value) {
    return brawn_from_number(-get_number(value));
}

brawn_value_t brawn_pos(brawn_value_t value) {
    return brawn_from_number(get_number(value));
}

brawn_value_t brawn_add(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(get_number(value1) + get_number(value2));
}

brawn_value_t brawn_sub(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(get_number(value1) - get_number(value2));
}

brawn_value_t brawn_mult(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(get_number(value1) * get_number(value2));
}

brawn_value_t brawn_div(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(get_number(value1) / get_number(value2));
}

brawn_value_t brawn_pow(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(std::pow(get_number(value1), get_number(value2)));
}

brawn_value_t brawn_mod(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_number(std::fmod(get_number(value1), get_number(value2)));
}

brawn_value_t brawn_and(brawn_value_t value1, brawn_value_t value2) {
    return boolean(brawn_is_true(value1) && brawn_is_true(value2));
}

brawn_value_t brawn_or(brawn_value_t value1, brawn_value_t value2) {
    return boolean(brawn_is_true(value1) || brawn_is_true(value2));
}

brawn_value_t brawn_lt(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) < get_number(value2));
    } else {
        return boolean(get_string(value1) < get_string(value2));
    }
}

brawn_value_t brawn_gt(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) > get_number(value2));
    } else {
        return boolean(get_string(value1) > get_string(value2));
    }
}

brawn_value_t brawn_le(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) <= get_number(value2));
    } else {
        return boolean(get_string(value1) <= get_string(value2));
    }
}

brawn_value_t brawn_ge(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) >= get_number(value2));
    } else {
        return boolean(get_string(value1) >= get_string(value2));
    }
}

brawn_value_t brawn_eq(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) == get_number(value2));
    } else {
        return boolean(get_string(value1) == get_string(value2));
    }
}

brawn_value_t brawn_ne(brawn_value_t value1, brawn_value_t value2) {
    BRAWN_SCALAR(value1);
    BRAWN_SCALAR(value2);
    if (numeric_comparision(value1, value2)) {
        return boolean(get_number(value1) != get_number(value2));
    } else {
        return boolean(get_string(value1) != get_string(value2));
    }
}

brawn_value_t brawn_concat(brawn_value_t value1, brawn_value_t value2) {
    return brawn_from_string(get_string(value1) + get_string(value2));
}

brawn_value_t brawn_match(brawn_value_t string, brawn_value_t pattern) {
    BRAWN_SCALAR(pattern);
    auto regex = std::regex(get_string(pattern), std::regex::awk);
    return brawn_match_regex(string, &regex);
}

brawn_value_t brawn_match_regex(brawn_value_t string, std::regex* pattern) {
    BRAWN_SCALAR(string);
    BRAWN_VALID(pattern);
    return boolean(std::regex_match(get_string(string), *pattern));
}

brawn_value_t brawn_not_match(brawn_value_t string, brawn_value_t pattern) {
    BRAWN_SCALAR(pattern);
    auto regex = std::regex(get_string(pattern), std::regex::awk);
    return brawn_not_match_regex(string, &regex);
}

brawn_value_t brawn_not_match_regex(brawn_value_t string, std::regex* pattern) {
    BRAWN_SCALAR(string);
    BRAWN_VALID(pattern);
    return boolean(!std::regex_match(get_string(string), *pattern));
}

brawn_value_t brawn_member(brawn_value_t value, brawn_value_t array) {
    BRAWN_SCALAR(value);
    BRAWN_ARRAY(array);
    return boolean(array->array_val->find(get_string(value)) != array->array_val->end());
}

brawn_value_t brawn_atan2(brawn_value_t y,brawn_value_t x) {
    BRAWN_SCALAR(x);
    BRAWN_SCALAR(y);
    return brawn_from_number(std::atan2(get_number(y), get_number(x)));
}

brawn_value_t brawn_cos(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::cos(get_number(x)));
}

brawn_value_t brawn_sin(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::sin(get_number(x)));
}

brawn_value_t brawn_exp(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::exp(get_number(x)));
}

brawn_value_t brawn_log(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::log(get_number(x)));
}

brawn_value_t brawn_sqrt(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::sqrt(get_number(x)));
}

brawn_value_t brawn_int(brawn_value_t x) {
    BRAWN_SCALAR(x);
    return brawn_from_number(std::trunc(get_number(x)));
}

brawn_value_t brawn_rand() {
    return brawn_from_number(static_cast<double>(std::rand()) / static_cast<double>(RAND_MAX));
}

/** Seed for the brawn runtime. */
static brawn_value_t curr_seed = brawn_from_number(1);

brawn_value_t brawn_srand(brawn_value_t seed) {
    // store the previous seed
    auto previous_seed = curr_seed;

    // if there is no argument given then
    // seed with the current time otherwise
    // seed the RNG with the given value
    // and return the previous seed
    if (seed == nullptr) {
        auto now_seed = std::time(0);
        curr_seed = brawn_from_number(now_seed);
        std::srand(now_seed);
    }
    else {
        BRAWN_SCALAR(seed);
        auto now_seed = get_number(seed);
        curr_seed = brawn_from_number(now_seed);
        std::srand(now_seed);
    }
    return previous_seed;
}

brawn_value_t brawn_string_index(brawn_value_t string, brawn_value_t find) {
    BRAWN_SCALAR(string);
    BRAWN_SCALAR(find);
    auto result = get_string(string).find(get_string(find));
    if (result == std::string::npos) {
        return zero;
    } else {
        return brawn_from_number(result + 1);
    }
}

brawn_value_t brawn_length(brawn_value_t string) {
    BRAWN_SCALAR(string);
    return brawn_from_number(get_string(string).size());
}

brawn_value_t brawn_gsub(brawn_value_t pattern, brawn_value_t replace, brawn_value_t input) {
    return nullptr;
}

brawn_value_t brawn_gsub_regex(std::regex* regex, brawn_value_t replace, brawn_value_t input) {
    return nullptr;
}

brawn_value_t brawn_match_position(brawn_value_t string, brawn_value_t pattern) {
    return nullptr;
}

brawn_value_t brawn_match_position_regex(brawn_value_t string, std::regex* regex) {
    return nullptr;
}

brawn_value_t brawn_split(brawn_value_t string, brawn_value_t array, brawn_value_t seperator) {
    return nullptr;
}

brawn_value_t brawn_string_sub(brawn_value_t pattern, brawn_value_t repl, brawn_value_t in) {
    return nullptr;
}

brawn_value_t brawn_string_sub_regex(std::regex* regex, brawn_value_t repl, brawn_value_t in) {
    return nullptr;
}

brawn_value_t brawn_substr(brawn_value_t string, brawn_value_t start, brawn_value_t end) {
    BRAWN_SCALAR(string);
    BRAWN_SCALAR(start);
    if (end != nullptr) { BRAWN_SCALAR(end); }
    auto s = get_string(string);
    auto m = get_number(start);
    auto n = end == nullptr ? s.size() : get_number(end);
    return brawn_from_string(s.substr(m - 1, n));
}

brawn_value_t brawn_tolower(brawn_value_t string) {
    BRAWN_SCALAR(string);
    auto s = get_string(string);
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::tolower(c); });
    return brawn_from_string(std::move(s));
}

brawn_value_t brawn_toupper(brawn_value_t string) {
    BRAWN_SCALAR(string);
    auto s = get_string(string);
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::toupper(c); });
    return brawn_from_string(std::move(s));
}

brawn_value_t brawn_system(brawn_value_t expression) {
    BRAWN_SCALAR(expression);
    return brawn_from_number(std::system(get_string(expression).c_str()));
}

brawn_value_t brawn_getline(brawn_value_t lvalue) {
    BRAWN_SCALAR(lvalue);
    return nullptr;
}

bool brawn_print(uint32_t count, ...) {
    std::va_list args;
    va_start(args, count);
    for (size_t i = 0; i < count; i++) {
        print_brawn_value(va_arg(args, brawn_value_t));
        print_brawn_value(OFS);
    }
    print_brawn_value(ORS);
    return true;
}

#undef BRAWN_VALID

#undef BRAWN_ASSERT

} // namespace brawn
