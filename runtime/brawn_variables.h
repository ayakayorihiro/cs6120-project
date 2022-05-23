#pragma once

#include "brawn_runtime.h"

namespace brawn {

extern "C" {

/** Declare all the inbuilt variables. */
extern brawn_value_t ARGC;
extern brawn_value_t ARGV;
extern brawn_value_t ENVIRON;
extern brawn_value_t FS;
extern brawn_value_t NF;
extern brawn_value_t NR;
extern brawn_value_t OFS;
extern brawn_value_t ORS;
extern brawn_value_t RLENGTH;
extern brawn_value_t RS;
extern brawn_value_t RSTART;
extern brawn_value_t SUBSEP;
extern brawn_value_t DOLLAR;

}

} // namespace brawn
