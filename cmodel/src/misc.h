#ifndef MISC_H
#define MISC_H

#include <stdint.h>

inline uint32_t float_as_int(float f)
{
    union {
        float       f;
        uint32_t    i;
    } fi;

    fi.f = f;

    return fi.i;
}

inline uint64_t double_as_long(double f)
{
    union {
        double      f;
        uint64_t    i;
    } fi;

    fi.f = f;

    return fi.i;
}


// https://codingforspeed.com/counting-the-number-of-leading-zeros-for-a-32-bit-integer-signed-or-unsigned/
inline int leading_zeros_int(int32_t x)
{
    unsigned n = 0;
    const unsigned bits = sizeof(x) * 8;
    for (int i = 1; i < bits; i ++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
}

inline int leading_zeros_long(int64_t x)
{
    unsigned n = 0;
    const unsigned bits = sizeof(x) * 8;
    for (int i = 1; i < bits; i ++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
}

#endif
