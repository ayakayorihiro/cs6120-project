#include <algorithm>
#include <cstdio>
#include <ios>
#include <iostream>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cctype>
#include <cstdarg>
#include <regex>
#include <sstream>
#include <string>

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
        value->array_val = new (GC) brawn_array;             \
        value->tag = ARRAY;                                  \
    }                                                        \
}                                                            \

namespace brawn {

/** Create some static constant brawn values. */
static brawn_value_t one = brawn_from_number(1);
static brawn_value_t zero = brawn_from_number(0);
static brawn_value_t empty_string = brawn_from_const_string("");
static brawn_value_t zero_string = brawn_from_const_string("0");
static std::regex default_fs("[\t\n ]+");

/* Create builtin variables. */
brawn_value_t ARGC = brawn_from_number(0);
brawn_value_t ARGV = brawn_init_array();
brawn_value_t ENVIRON = brawn_init_array();
brawn_value_t FNR = brawn_from_number(0);
brawn_value_t FS = brawn_from_const_string(" ");
brawn_value_t NF = brawn_from_number(0);
brawn_value_t NR = brawn_from_number(0);
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
static inline brawn_value_t brawn_from_string_copy(std::string string) {
    auto value = new (GC) brawn_value;
    value->string_val = new (GC) std::string(string);
    value->tag = STRING;
    return value;
}

/**
 * Initialise a brawn value from a temporary string.
 *
 * @param string the string
 *
 * @return a brawn value containing that string
 */
static inline brawn_value_t brawn_from_string_temp(std::string&& string) {
    auto value = new (GC) brawn_value;
    value->string_val = new (GC) std::string(std::move(string));
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
static inline std::string get_string(brawn_value_t value) {
    BRAWN_SCALAR(value);
    switch (value->tag) {
        case UNINITIALISED:
            return *empty_string->string_val;
        case NUMBER:
            double int_part;
            if (std::modf(value->number_val, &int_part) == 0.) {
                return std::string(std::to_string(static_cast<long long>(int_part)));
            } else {
                return std::string(std::to_string(value->number_val));
            }
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
static inline void print_brawn_value(brawn_value_t value) {
    BRAWN_SCALAR(value);
    switch (value->tag) {
        case NUMBER:
            std::cout << value->number_val;
            return;
        case STRING:
            std::cout << *value->string_val;
            return;
        case UNINITIALISED:
        default:
            return;
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
    return new (GC) brawn_value();
}

brawn_value_t brawn_init_array() {
    auto value = new (GC) brawn_value();
    value->array_val = new (GC) brawn_array;
    value->tag = ARRAY;
    return value;
}

brawn_value_t brawn_from_number(double number) {
    auto value = new (GC) brawn_value();
    value->number_val = number;
    value->tag = NUMBER;
    return value;
}

brawn_value_t brawn_from_const_string(const char* string) {
    auto value = new (GC) brawn_value();
    value->string_val = new (GC) std::string(string);
    value->tag = STRING;
    return value;
}

std::regex* brawn_init_regex(const char* pattern) {
    return new (GC) std::regex(pattern, std::regex::extended);
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
        return (*array->array_val)[key] = new (GC) brawn_value();
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
    return new (GC) brawn_iterator { array->array_val, array->array_val->begin() };
}

brawn_value_t brawn_next_iterator(brawn_iterator* iterator) {
    // get the next value in the iterator
    BRAWN_VALID(iterator);
    BRAWN_VALID(iterator->array);
    if (iterator->iterator == iterator->array->end()) {
        return nullptr;
    } else {
        auto key = ((iterator->iterator)++)->first;
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

brawn_value_t brawn_subtr(brawn_value_t value1, brawn_value_t value2) {
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
    return brawn_from_string_temp(get_string(value1) + get_string(value2));
}

brawn_value_t brawn_match_builtin(brawn_value_t string, brawn_value_t pattern) {
    BRAWN_SCALAR(pattern);
    auto regex = std::regex(get_string(pattern), std::regex::extended);
    return brawn_match_builtin_regex(string, &regex);
}

brawn_value_t brawn_match_builtin_regex(brawn_value_t string, std::regex* pattern) {
    BRAWN_SCALAR(string);
    BRAWN_VALID(pattern);
    return boolean(std::regex_search(get_string(string), *pattern));
}

brawn_value_t brawn_not_match(brawn_value_t string, brawn_value_t pattern) {
    BRAWN_SCALAR(pattern);
    auto regex = std::regex(get_string(pattern), std::regex::extended);
    return brawn_not_match_regex(string, &regex);
}

brawn_value_t brawn_not_match_regex(brawn_value_t string, std::regex* pattern) {
    BRAWN_SCALAR(string);
    BRAWN_VALID(pattern);
    return boolean(!std::regex_search(get_string(string), *pattern));
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

brawn_value_t brawn_index(brawn_value_t string, brawn_value_t find) {
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

brawn_value_t brawn_match(brawn_value_t string, brawn_value_t pattern) {
    BRAWN_SCALAR(string);
    BRAWN_SCALAR(pattern);
    auto regex = std::regex(get_string(pattern), std::regex::extended);
    return brawn_match_regex(string, &regex);
}

brawn_value_t brawn_match_regex(brawn_value_t string, std::regex* regex) {
    BRAWN_SCALAR(string);
    BRAWN_VALID(regex);

    // get the string and create the match object
    auto str = get_string(string);
    auto match = std::smatch();
    long index, length;

    // perform the match and extract the results
    std::regex_search(str, match, *regex);
    if (match.empty()) {
        index = 0;
        length = -1;
    } else {
        index = match.position(0) + 1;
        length = match.length(0);
    }

    // set the built-in variables and return
    RSTART = brawn_from_number(index);
    RLENGTH = brawn_from_number(length);
    return RSTART;
}

brawn_value_t brawn_split(brawn_value_t string, brawn_value_t array, brawn_value_t seperator) {
    BRAWN_SCALAR(string);
    BRAWN_ARRAY(array);
    if (seperator != nullptr) { BRAWN_SCALAR(seperator); }
    auto field_sep = get_string(seperator);
    if (field_sep == " ") {
        return brawn_split_regex(string, array, &default_fs);
    } else {
        auto regex = std::regex(get_string(seperator), std::regex::extended);
        return brawn_split_regex(string, array, &regex);
    }
}

brawn_value_t brawn_split_regex(brawn_value_t string, brawn_value_t array, std::regex* regex) {
    BRAWN_SCALAR(string);
    BRAWN_ARRAY(array);
    BRAWN_VALID(regex);

    // perform the match on the string
    auto record = get_string(string);
    auto begin = std::sregex_iterator(record.begin(), record.end(), *regex);
    auto end = std::sregex_iterator();

    // split the record into different fields
    size_t num_records = 1;
    std::string last = record;
    for (auto it = begin; it != end; ++it) {
        if (!it->prefix().str().empty()) {
            brawn_update_array(
                array,
                brawn_from_string_temp(std::to_string(num_records)),
                brawn_from_string_temp(it->prefix().str())
            );
            last = it->suffix().str();
            num_records += 1;
        }
    }
    brawn_update_array(
        array,
        brawn_from_string_temp(std::to_string(num_records)),
        brawn_from_string_temp(std::move(last))
    );
    return brawn_from_number(num_records);
}

brawn_value_t brawn_gsub(brawn_value_t pattern, brawn_value_t replace, brawn_value_t input) {
    BRAWN_SCALAR(pattern);
    BRAWN_SCALAR(replace);
    BRAWN_SCALAR(input);
    auto regex = std::regex(get_string(pattern), std::regex::extended);
    return brawn_sub_regex(&regex, pattern, input);
}

brawn_value_t brawn_gsub_regex(std::regex* regex, brawn_value_t replace, brawn_value_t input) {
    BRAWN_VALID(regex);
    BRAWN_SCALAR(replace);
    BRAWN_SCALAR(input);

    // perform the replacement
    auto record = get_string(input), repl = get_string(replace);
    auto begin = std::sregex_iterator(record.begin(), record.end(), *regex);
    auto end = std::sregex_iterator();

    // perform replacement on each match
    size_t num_subs = 0;
    auto last = record;
    auto result = new (GC) std::string();
    for (auto it = begin; it != end; ++it) {
        result->append(it->prefix().str());
        result->append(it->format(repl, std::regex_constants::format_sed));
        last = it->suffix().str();
        num_subs += 1;
    }
    result->append(last);

    // set the value of input and return
    input->tag = STRING;
    input->string_val = result;
    return brawn_from_number(num_subs);
}

brawn_value_t brawn_sub(brawn_value_t pattern, brawn_value_t replace, brawn_value_t input) {
    BRAWN_SCALAR(pattern);
    BRAWN_SCALAR(replace);
    BRAWN_SCALAR(input);
    auto regex = std::regex(get_string(pattern), std::regex::extended);
    return brawn_sub_regex(&regex, pattern, input);
}

brawn_value_t brawn_sub_regex(std::regex* regex, brawn_value_t replace, brawn_value_t input) {
    BRAWN_VALID(regex);
    BRAWN_SCALAR(replace);
    BRAWN_SCALAR(input);

    // perform the replacement
    auto record = get_string(input), repl = get_string(replace);
    auto begin = std::sregex_iterator(record.begin(), record.end(), *regex);
    auto end = std::sregex_iterator();

    // perform replacement on each match
    size_t num_subs = 0;
    auto last = record;
    auto result = new (GC) std::string();
    for (auto it = begin; it != end; ++it) {
        result->append(it->prefix().str());
        result->append(it->format(repl, std::regex_constants::format_sed));
        last = it->suffix().str();
        num_subs += 1;
        break; // only perform at most one substitution
    }
    result->append(last);

    // set the value of input and return
    input->tag = STRING;
    input->string_val = result;
    return brawn_from_number(num_subs);
}

brawn_value_t brawn_substr(brawn_value_t string, brawn_value_t start, brawn_value_t end) {
    BRAWN_SCALAR(string);
    BRAWN_SCALAR(start);
    if (end != nullptr) { BRAWN_SCALAR(end); }
    auto s = get_string(string);
    auto m = get_number(start);
    auto n = end == nullptr ? s.size() : get_number(end);
    return brawn_from_string_temp(s.substr(m - 1, n));
}

brawn_value_t brawn_tolower(brawn_value_t string) {
    BRAWN_SCALAR(string);
    auto s = get_string(string);
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::tolower(c); });
    return brawn_from_string_temp(std::move(s));
}

brawn_value_t brawn_toupper(brawn_value_t string) {
    BRAWN_SCALAR(string);
    auto s = get_string(string);
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::toupper(c); });
    return brawn_from_string_temp(std::move(s));
}

brawn_value_t brawn_system(brawn_value_t expression) {
    BRAWN_SCALAR(expression);
    return brawn_from_number(std::system(get_string(expression).c_str()));
}

brawn_value_t brawn_getline(brawn_value_t lvalue) {
    if (lvalue != nullptr) { BRAWN_SCALAR(lvalue); }

    // get the record seperator and it's length
    char record_sep = '\n';
    auto rs = get_string(RS);
    if (!rs.empty()) {
        record_sep = rs[0];
    }

    // create a buffer for the final string
    auto buf = std::stringbuf();

    // read characters till we get an EOF or
    // record seperator
    while (std::cin) {
        decltype(std::cin)::int_type c = std::cin.get();
        if (c == std::char_traits<char>::eof() || c == record_sep) {
            break;
        } else {
            // add the read character to string buffer
            buf.sputc(c);
        }
    }

    // if no characters were read, then return false
    if (buf.in_avail() == 0) {
        return zero;
    }

    // extract the string
    auto record = buf.str();

    // if the lvalue is not null, then we
    // set it's value to be the acquired string
    // otherwise we update the DOLLAR variable
    if (lvalue != nullptr) {
        lvalue->tag = STRING;
        lvalue->string_val = new (GC) std::string(record);
    } else {
        // set the value for $0 and others using `brawn_split_regex`
        auto val = brawn_from_string_temp(buf.str());
        brawn_update_array(DOLLAR, zero_string, val);
        brawn_split(val, DOLLAR, FS);
    }

    // since a record was read, return true
    return one;
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

/** array of strings for environment variables */
extern char **environ;

/**
 * Setup the environment variables.
 */
static inline void setup_environment() {
    for (size_t index = 0; environ[index] != nullptr; index++) {
        auto str = std::string(environ[index]);
        auto split = str.find_first_of('=');
        if (split != std::string::npos) {
            auto key = str.substr(0, split), value = str.substr(split);
            brawn::brawn_update_array(
                brawn::ENVIRON,
                brawn::brawn_from_string_temp(std::move(key)),
                brawn::brawn_from_string_temp(std::move(value))
            );
        }
    }
}

/**
 * Setup the arguments.
 */
static inline void setup_args(int argc, char* argv[]) {
    brawn::ARGC = brawn::brawn_from_number(argc);
    for (size_t index = 0; index < argc; index++) {
        brawn::brawn_update_array(
            brawn::ARGV,
            brawn::brawn_from_string_temp(std::to_string(index)),
            brawn::brawn_from_const_string(argv[index])
        );
    }
}

/**
 * The entry point for the awk program
 */
int main (int argc, char* argv[]) {
    // setup environ and arg
    setup_environment();
    setup_args(argc, argv);

    // return value for the function
    int result = 0;

    // whether to skip the main loop
    bool skip = false;

    // perform the begin actions
    try {
        brawn::brawn_begin();
    } catch (const brawn::BrawnExitException& e) {
        if (e.value != nullptr) {
            result = brawn::get_number(e.value);
        }
        skip = true;
    }

    // perform the process loop
    while (!skip) {
        // get the next line to process
        auto value = brawn::brawn_getline(nullptr);
        if (!brawn::brawn_is_true(value)) {
            break;
        }

        // perform an iteration of the 
        try {
            brawn::brawn_process();
        } catch (const brawn::BrawnNextException& e) {
            continue;
        } catch (const brawn::BrawnExitException& e) {
            if (e.value != nullptr) {
                result = brawn::get_number(e.value);
            }
            break;
        }
    }

    // perform the end actions
    try {
        brawn::brawn_end();
    } catch (const brawn::BrawnExitException& e) {
        if (e.value != nullptr) {
            result = brawn::get_number(e.value);
        }
    }

    // return the exit value
    return result;
}
