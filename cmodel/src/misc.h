#ifndef MISC_H
#define MISC_H

inline unsigned int float_as_int(float f)
{
    union {
        float       f;
        unsigned    i;
    } fi;

    fi.f = f;

    return fi.i;
}

// https://codingforspeed.com/counting-the-number-of-leading-zeros-for-a-32-bit-integer-signed-or-unsigned/
inline int leading_zeros(int x)
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
