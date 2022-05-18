#pragma once

#include <string>
#include <unordered_map>
#include <regex>
#include <functional>

#include "deps/gc/include/gc_cpp.h"
#include "deps/gc/include/gc_allocator.h"

namespace brawn {

extern "C" {

/** Forward declaration for a brawn value. */
struct brawn_value;

/** The type of a brawn value. */
typedef brawn_value* brawn_value_t;

/** This is the representation of a brawn string. */
typedef std::basic_string
<
    char,
    std::char_traits<char>,
    gc_allocator<char>
> brawn_string;

/** Type of a brawn string. */
typedef brawn_string* brawn_string_t;

/** Rrepresentation of a brawn "array". */
typedef std::unordered_map
<
    brawn_string,
    brawn_value_t,
    std::hash<brawn_string>,
    std::equal_to<brawn_string>,
    gc_allocator<std::pair<const brawn_string, brawn_value_t>>
> brawn_array;

/** Type of a brawn "array". */
typedef brawn_array* brawn_array_t;

/**
 * Type tage for each brawn type.
 */
enum brawn_type_t {
    UNINITIALISED = 0,
    NUMBER,
    STRING,
    ARRAY
};

/**
 * A object representing a brawn value.
 */
struct brawn_value {
    brawn_type_t tag;              /** type tag for this value */
    union {
        brawn_string_t string_val; /** the container for the string */
        brawn_array_t  array_val;  /** the container for the hashmap */
        double         number_val; /** the container for the number */
    };

    /** 
     * Simple initialiser for a brawn value.
     */
    brawn_value(): tag(UNINITIALISED) {}

};

/**
 * Return an uninitialised brawn value.
 */
brawn_value_t brawn_init();

/**
 * Initialise a brawn value to be an array.
 *
 * @return a brawn vaue with an array
 */
brawn_value_t brawn_init_array();

/**
 * Initialise a brawn value from a number.
 *
 * @param number the float
 *
 * @return a brawn value containing that number
 */
brawn_value_t brawn_from_number(double number);

/**
 * Initialise a brawn value from a C string.
 *
 * @param string the string
 *
 * @return a brawn value containing that C string
 */
brawn_value_t brawn_from_const_string(const char* string);

/**
 * Initialise a brawn value from a string.
 *
 * @param string the string
 *
 * @return a brawn value containing that string
 */
brawn_value_t brawn_from_string(std::string string);

/**
 * Initialise a regular expression from a string.
 *
 * @param pattern the pattern as a const string
 *
 * @return initialise that regex
 */
std::regex* brawn_init_regex(const char* pattern);

/**
 * Test a brawn value for truthiness.
 *
 * @param value
 *
 * @return truthiness
 */
bool brawn_is_true(brawn_value_t value);

/**
 * Assign a brawn value from another brawn value.
 *
 * @param lvalue the asignee
 * @param value  the value assigned
 *
 * @return the assigned brawn value
 */
brawn_value_t brawn_assign(brawn_value_t lvalue, brawn_value_t value);

/**
 * Index into the given brawn array and return the value.
 *
 * @param array the brawn array
 * @param index the index
 *
 * @return the value at the index
 */
brawn_value_t brawn_index(brawn_value_t array, brawn_value_t index);

/**
 * Perform a logical not on the brawn value.
 *
 * @param value the brawn value
 *
 * @return the logical not value
 */
brawn_value_t brawn_not(brawn_value_t value);

/**
 * Negate the given brawn value.
 *
 * @param value
 *
 * @return the negated value
 */
brawn_value_t brawn_neg(brawn_value_t value);

/**
 * Add the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the sum
 */
brawn_value_t brawn_add(brawn_value_t value1, brawn_value_t value2);

/**
 * Subtract the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the subtracted result
 */
brawn_value_t brawn_sub(brawn_value_t value1, brawn_value_t value2);

/**
 * Multiply the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the product
 */
brawn_value_t brawn_mult(brawn_value_t value1, brawn_value_t value2);

/**
 * Divide the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the result of division
 */
brawn_value_t brawn_div(brawn_value_t value1, brawn_value_t value2);

/**
 * Calculate the exponent of the two brawn value.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the exponent
 */
brawn_value_t brawn_pow(brawn_value_t value1, brawn_value_t value2);

/**
 * Calculate the remainder of the two brawn value.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the remainder
 */
brawn_value_t brawn_mod(brawn_value_t value1, brawn_value_t value2);

/**
 * Perform the logical and of the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the logical and
 */
brawn_value_t brawn_and(brawn_value_t value1, brawn_value_t value2);

/**
 * Perform the logical or of the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the logical or
 */
brawn_value_t brawn_or(brawn_value_t value1, brawn_value_t value2);

/**
 * Compare the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return result of the less-than comparision
 */
brawn_value_t brawn_lt(brawn_value_t value1, brawn_value_t value2);

/**
 * Compare the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return result of the greater-than comparision
 */
brawn_value_t brawn_gt(brawn_value_t value1, brawn_value_t value2);

/**
 * Compare the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return result of the less-than-or-equal-to comparision
 */
brawn_value_t brawn_le(brawn_value_t value1, brawn_value_t value2);

/**
 * Compare the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return result of the greater-than-or-equal-to comparision
 */
brawn_value_t brawn_ge(brawn_value_t value1, brawn_value_t value2);

/**
 * Are the two brawn values equal?
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return whether the two brawn values are equal
 */
brawn_value_t brawn_eq(brawn_value_t value1, brawn_value_t value2);

/**
 * Are the two brawn values not equal?
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return whether the two brawn values are not equal
 */
brawn_value_t brawn_ne(brawn_value_t value1, brawn_value_t value2);

/**
 * Concatenate the two brawn values.
 *
 * @param value1 the first value
 * @param value2 the second value
 *
 * @return the concatenation
 */
brawn_value_t brawn_concat(brawn_value_t value1, brawn_value_t value2);

/**
 * Perform pattern matching on the two brawn values.
 *
 * @param string  the string to match
 * @param pattern the pattern
 *
 * @return whether the pattern matches to the string
 */
brawn_value_t brawn_match(brawn_value_t string, brawn_value_t pattern);

/**
 * Match the given brawn value to the regex.
 *
 * @param string  the string to match
 * @param pattern the pattern
 *
 * @return whether the pattern matches to the string
 */
brawn_value_t brawn_match_regex(brawn_value_t string, std::regex pattern);

/**
 * Brawn inbuild function: atan2.
 *
 * @param y the first parameter
 * @param x the second parameter
 *
 * @return result of atan2
 */
brawn_value_t brawn_atan2(brawn_value_t y,brawn_value_t x);

/**
 * Brawn inbuild function: cos.
 *
 * @param x the first parameter
 *
 * @return result of cos
 */
brawn_value_t brawn_cos(brawn_value_t x);

/**
 * Brawn inbuild function: sin.
 *
 * @param x the first parameter
 *
 * @return result of sin
 */
brawn_value_t brawn_sin(brawn_value_t x);

/**
 * Brawn inbuild function: exp.
 *
 * @param x the first parameter
 *
 * @return result of exp
 */
brawn_value_t brawn_exp(brawn_value_t x);

/**
 * Brawn inbuild function: log.
 *
 * @param x the first parameter
 *
 * @return result of log
 */
brawn_value_t brawn_log(brawn_value_t x);

/**
 * Brawn inbuild function: sqrt.
 *
 * @param x the first parameter
 *
 * @return result of sqrt
 */
brawn_value_t brawn_sqrt(brawn_value_t x);

/**
 * Brawn inbuild function: int.
 *
 * @param x the first parameter
 *
 * @return result of int
 */
brawn_value_t brawn_int(brawn_value_t x);

/**
 * Brawn inbuild function: rand.
 *
 * @return the generated random number
 */
brawn_value_t brawn_rand();

/**
 * Brawn inbuild function: srand. Seed the
 * RNG with the current time of day.
 *
 * @return the previous seed
 */
brawn_value_t brawn_srand_time();

/**
 * Brawn inbuild function: srand.
 *
 * @param seed the seed for the srand call
 *
 * @return the previous seed
 */
brawn_value_t brawn_srand(brawn_value_t seed);

// brawn_value_t brawn_gsub(brawn_value_t pattern, brawn_value_t replace, brawn_value_t input);

// brawn_value_t brawn_gsub_regex(std::regex pattern, brawn_value_t replace, brawn_value_t input);

/**
 * Return the index of the string 'find'
 * in the string 'string'.
 *
 * @param string the given string
 * @param find   the substring to find
 *
 * @return the index starting from 1
 */
brawn_value_t brawn_string_index(brawn_value_t string, brawn_value_t find);

/**
 * Return the length of the given string.
 *
 * @param string the given string
 *
 * @return length
 */
brawn_value_t brawn_length(brawn_value_t string);

// brawn_value_t brawn_match_position(brawn_value_t string, brawn_value_t pattern);

// brawn_value_t brawn_split(brawn_value_t string, brawn_value_t array, brawn_value_t seperator);

// brawn_value_t brawn_string_sub(ere, repl[, in  ]);

/**
 * Return the substring of the given string.
 *
 * @param string the given string
 * @param start  the starting position
 * @param end    the ending position
 *
 * @return the substring
 */
brawn_value_t brawn_substr(brawn_value_t string, brawn_value_t start, brawn_value_t end = nullptr);

/**
 * Convert the string to lowercase.
 *
 * @param string the given string
 *
 * @return lowercase string
 */
brawn_value_t brawn_tolower(brawn_value_t string);

/**
 * Convert the string to uppercase.
 *
 * @param string the given string
 *
 * @return uppercase string
 */
brawn_value_t brawn_toupper(brawn_value_t string);

/**
 * Perform a system call.
 *
 * @param expression the argument to the system call
 *
 * @return the return value
 */
brawn_value_t brawn_system(brawn_value_t expression);

} // extern "C"

} // namespace brawn
