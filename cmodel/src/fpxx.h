#ifndef FPXX_H
#define FPXX_H

#include <iostream>

#include "misc.h"

#define FPXX_SQRT_LUT_SIZE_BITS          12
#define FPXX_SQRT_LUT_SIZE               ((1L<<FPXX_SQRT_LUT_SIZE_BITS)-(1L<<(FPXX_SQRT_LUT_SIZE_BITS-2)))
#define FPXX_SQRT_LUT_MANT_BITS          12
#define FPXX_SQRT_LUT_SHIFT_BITS         4
#define FPXX_SQRT_FRAC_BITS              8

#define FP32_MANT_BITS                  23
#define FP32_MANT_MASK                  ((1L<<FP32_MANT_BITS)-1)
#define FP32_EXP_BITS                   8
#define FP32_EXP_MASK                   ((1L<<FP32_EXP_BITS)-1)

#define FP64_MANT_BITS                  52L
#define FP64_MANT_MASK                  ((1L<<FP64_MANT_BITS)-1)
#define FP64_EXP_BITS                   11L
#define FP64_EXP_MASK                   ((1L<<FP64_EXP_BITS)-1)

typedef struct {
    uint64_t    mant;
    int         shift;
} sqrt_lut_entry_t;

typedef struct {
    uint64_t    mant;
    int         shift;
} div_lut_entry_t;

template <int _m_size, int _exp_size, int _zero_offset = ((1L<<(_exp_size-1))-1)>
class fpxx {

public:
    bool        sign;
    int32_t     exp;
    uint64_t    m;

    fpxx() {
        sign    = 0;
        exp     = 0;
        m       = 0;
    }

    void float_to_this(float f) {
        union {
            float       f;
            uint32_t    i;
        } fi;

        fi.f = f;

        bool     f_s    = fi.i >> (FP32_MANT_BITS+FP32_EXP_BITS);
        uint32_t f_e    = (fi.i >> FP32_MANT_BITS) & FP32_EXP_MASK;
        uint64_t f_m    = fi.i & FP32_MANT_MASK;

        sign    = f_s;
        exp     = f_e == 0 ? 0 : (f_e - ((1L<<(FP32_EXP_BITS-1))-1) + _zero_offset);
        exp     = exp & ((1L<<_exp_size)-1);

        m       = _m_size > FP32_MANT_BITS ? f_m << (_m_size-FP32_MANT_BITS) : f_m >> (FP32_MANT_BITS-_m_size);
    }

    void double_to_this(double f) {
        union {
            double       f;
            uint64_t    i;
        } fi;

        fi.f = f;

        bool     f_s    = fi.i >> (FP64_MANT_BITS+FP64_EXP_BITS);
        uint64_t f_e    = (fi.i >> FP64_MANT_BITS) & FP64_EXP_MASK;
        uint64_t f_m    = fi.i & FP64_MANT_MASK;

        sign    = f_s;
        exp     = f_e == 0 ? 0 : (f_e - ((1L<<(FP64_EXP_BITS-1))-1) + _zero_offset);
        exp     = exp & ((1L<<_exp_size)-1);

        m       = _m_size > FP64_MANT_BITS ? f_m << (_m_size-FP64_MANT_BITS) : f_m >> (FP64_MANT_BITS-_m_size);
    }


    fpxx(float f){
        float_to_this(f);
    }

    fpxx operator=(float f) {

        float_to_this(f);

        return *this;
    }

    float to_float() const {
        bool round_ena = false;

        int e = exp == 0 ? 0 : (exp - _zero_offset + ((1L<<(FP32_EXP_BITS-1))-1) ) & FP32_EXP_MASK;

        union {
            float       f;
            uint32_t    i;
        } fi;

        uint32_t mant_round;
        mant_round = (_m_size > FP32_MANT_BITS ? (m >> (_m_size-FP32_MANT_BITS)) + ((m >> (_m_size-FP32_MANT_BITS-1)) & round_ena)
                                               :  m << (FP32_MANT_BITS-_m_size));

        fi.i = (sign << (FP32_EXP_BITS+FP32_MANT_BITS)) | (e<<FP32_MANT_BITS) | mant_round;

        return fi.f;
    }

    double to_double() const {
        bool round_ena = false;

        int e = exp == 0 ? 0 : (exp - _zero_offset + ((1L<<(FP64_EXP_BITS-1))-1) ) & FP64_EXP_MASK;

        union {
            double      f;
            uint64_t    i;
        } fi;

        uint64_t mant_round;
        mant_round = (_m_size > FP64_MANT_BITS ? (m >> (_m_size-FP64_MANT_BITS)) + ((m >> (_m_size-FP64_MANT_BITS-1)) & round_ena)
                                               :  m << (FP64_MANT_BITS-_m_size));


        fi.i = ((uint64_t)sign << (FP64_EXP_BITS+FP64_MANT_BITS)) | ((uint64_t)e<<FP64_MANT_BITS) | mant_round;

        return fi.f;
    }

    operator float () const {
        return to_float();
    }

    void set_zero() {
        sign = 0;
        exp  = 0;
        m    = 0;
    }

    int zero_offset() {
        return _zero_offset;
    }

    int m_size() {
        return _m_size;
    }

    int exp_size() {

        return _exp_size;
    }

    bool is_zero() const {
        return exp == 0;
    }

    int mant() const {
        return m;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator+(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {
        fpxx<_m_size, _exp_size, _zero_offset> r;

        if (left.is_zero())
            return right;

        if (right.is_zero())
            return left;

        uint64_t m_left  = (1L << _m_size) | left.m;
        uint64_t m_right = (1L << _m_size) | right.m;

        int      s_add;
        int      e_add;
        uint64_t m_add;

        if (left.exp > right.exp){
            e_add   = left.exp;
            m_right = (left.exp - right.exp) >= 64 ? 0 : m_right >> (left.exp - right.exp);
        }
        else{
            e_add   = right.exp;
            m_left  = (right.exp - left.exp) >= 64 ? 0 : m_left  >> (right.exp - left.exp);
        }

        if (left.sign == right.sign){
            s_add = left.sign;
            m_add = m_left + m_right;
        }
        else{
            if (m_left > m_right){
                s_add = left.sign;
                m_add = m_left - m_right;
            }
            else{
                s_add = right.sign;
                m_add = m_right - m_left;
            }
        }

        if (m_add & (1L<<(_m_size+1))){
            e_add += 1;
            m_add >>= 1;
        }
        else{
            while((m_add & (1L<<_m_size)) == 0 && e_add != 0){
                e_add -= 1;
                m_add <<= 1;
            }
        }

        r.sign = s_add;
        r.exp  = e_add;
        r.m    = m_add & ((1L<<_m_size)-1);

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator*(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {
        fpxx<_m_size, _exp_size, _zero_offset> r;

        if (left.is_zero() || right.is_zero()){
            r.set_zero();
            return r;
        }

        uint64_t m_left  = (1L << _m_size) | left.m;
        uint64_t m_right = (1L << _m_size) | right.m;

        int      s_mul = left.sign ^ right.sign;
        int      e_mul = ((left.exp - _zero_offset) + (right.exp - _zero_offset)) + 1 + _zero_offset;
        uint64_t m_mul;

        m_mul = m_left * m_right;
        m_mul >>= (_m_size+1);

        if (e_mul < 0){
            r.set_zero();
            return r;
        }

        if (m_mul & (1L<<(_m_size+1))){
            e_mul += 1;
            m_mul >>= 1;
        }
        else{
            while((m_mul & (1L<<_m_size)) == 0 && e_mul != 0){
                e_mul -= 1;
                m_mul <<= 1;
            }
        }

        r.sign = s_mul;
        r.exp  = e_mul;
        r.m    = m_mul & ((1L<<_m_size)-1);

        return r;
    }

    friend bool operator< (const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        bool r = left.to_double() < right.to_double();

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator/(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        bool round_ena = true;

        assert((_m_size&1) == 1);

        int half_bits = (_m_size+1)/2;
        int lut_mant_bits = 2*half_bits+1;

        static bool init = false;
        static div_lut_entry_t div_lut[1L<<((_m_size+1)/2)];

        if (!init){
            int max_shift = 0;
            int table_size_bits = half_bits;
            int table_size = 1L<<table_size_bits;

            for(int i=0;i<table_size;++i){
                double fin   = 1.0 + (double)(i)/(double)table_size;
                int fin_exp = (double_as_long(fin) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                double      f     = 1.0/(fin*fin);
                uint64_t    f_int = double_as_long(f);

                uint64_t mant =  double_as_long(f) & FP64_MANT_MASK;
                int exp       = (double_as_long(f) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                int shift = fin_exp - exp;

                int round = (mant >> (FP64_MANT_BITS-(lut_mant_bits+1))) & 1;

                mant = mant >> (FP64_MANT_BITS-lut_mant_bits);
                mant += round & round_ena;

                div_lut[i].mant  = mant;
                div_lut[i].shift = shift;

                max_shift = shift > max_shift ? shift : max_shift;
            }
            init = true;
        }

        unsigned int yl_mask = (1L<<(half_bits-1))-1;
        unsigned int yh_mask = ~yl_mask & ((1L<<(2*half_bits))-1);

        uint64_t yh = ((1L<<(2*half_bits-1)) | right.m) & yh_mask;
        uint64_t yl = right.m & yl_mask;

        uint64_t yh_m_yl = yh - yl;

        int lut_addr = right.m >> (half_bits-1);   // lut size is 1L<<half_bits, and assumes no leading 1 included.

        div_lut_entry_t div_lut_val = div_lut[lut_addr];

        uint64_t recip_yh2 = (1L<<lut_mant_bits) | div_lut_val.mant;

        // Multiplying 2*half_bits * 2*half_bits = 4*half_bits.
        // According to paper, we need to keep 2*half_bits+2, so shift right by 2*half_bits-2.
        // However, in practice, it turns out that we need to keep one more bit, so shift by 2*half_bits-3 instead.
        __int128_t x_mul_yhyl = ((1L<<(2*half_bits-1)) | left.m) * yh_m_yl;
        int x_mul_yhyl_shift = 2*half_bits-3;
        __int128_t x_mul_yhyl_round = (x_mul_yhyl_round >> (x_mul_yhyl_shift+1)) & 1;
        x_mul_yhyl >>= x_mul_yhyl_shift;
        x_mul_yhyl += x_mul_yhyl_round & round_ena;

        // (2*half_bits+3) + (2*half_bits+2) = 4*half_bits + 5
        // We need to keep 2*half_bits eventually, but there may be a leading zero. So first go to 2*half_bits+2.
        // So shift by 2*half_bits + 3
        __int128_t div = (x_mul_yhyl * recip_yh2);
        int div_shift = lut_mant_bits+3;
        __int128_t div_round = (div >> (div_shift-1)) & 1;
        div >>= div_shift;
        div += div_round & round_ena;

        fpxx<_m_size, _exp_size, _zero_offset> r;

        if (left.is_zero()){
            r.sign = 0;
            r.exp = 0;
            r.m = 0;

            return r;
        }

        r.sign = left.sign ^ right.sign;

        int exp =  left.exp - right.exp + 1 - div_lut_val.shift + _zero_offset;


        unsigned int div_msb = 2*half_bits;
        if (div & (1L<<div_msb)){
            // div is 1x.xxxxxx. Shift right to 1.xxxxxx
            div >>= 1;
            exp += 1;
        }
        else if (   !(div & (1L<<(div_msb-1)))
                 &&  (div & (1L<<(div_msb-2))) ){
            // div is 00.1xxxxx. Shift left to 1.xxxxxx
            div <<= 1;
            exp -= 1;
        }
        else if (   !(div & (1L<<(div_msb-1)))
                 && !(div & (1L<<(div_msb-2)))
                 &&  (div & (1L<<(div_msb-3))) ){
            // div is 00.01xxxx. Shift left to 1.xxxxxx
            div <<= 2;
            exp -= 2;
        }

        if (exp > ((1L<<_exp_size)-1))
            exp = (1L<<_exp_size)-1;
        else if (exp <= 0){
            exp = 0;
            div = 0;
        }

        r.m = div & ((1L<<_m_size)-1);
        r.exp = exp;

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset> sqrt(const fpxx<_m_size, _exp_size, _zero_offset> op){

        static bool init = false;
        static sqrt_lut_entry_t sqrt_lut[FPXX_SQRT_LUT_SIZE];

        if (!init){
            int max_shift = 0;
            for(int i=0;i<FPXX_SQRT_LUT_SIZE;++i){
                double fin     = (double)((1L<<(FPXX_SQRT_LUT_SIZE_BITS-2)) + i)/(1L<<(FPXX_SQRT_LUT_SIZE_BITS-1));

                int fin_exp = (double_as_long(fin) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                double f      = sqrt(fin);

                uint64_t mant =  double_as_long(f) & FP64_MANT_MASK;
                int exp       = (double_as_long(f) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                int shift = fin_exp - exp;

                sqrt_lut[i].mant  = mant >> (FP64_MANT_BITS-FPXX_SQRT_LUT_MANT_BITS);
                sqrt_lut[i].shift = shift;

                max_shift = shift > max_shift ? shift : max_shift;
            }
            init = true;
        }

        int gt_1 = !((op.exp - _zero_offset) & 1);

        int lut_addr = (op.m | (1L<<_m_size)) << gt_1;
        lut_addr = lut_addr >> (_m_size+2-FPXX_SQRT_LUT_SIZE_BITS);
        lut_addr = lut_addr - (1L<<(FPXX_SQRT_LUT_SIZE_BITS-2));

        sqrt_lut_entry_t lut_val = sqrt_lut[lut_addr];

        fpxx<_m_size, _exp_size, _zero_offset> r;

        r.sign  = false;
        r.exp   = (op.exp - _zero_offset)/2 - lut_val.shift + _zero_offset;
        if (r.exp < 0)
            r.exp = 0;
        if (r.exp > (1L<<_exp_size)-2)
            r.exp = (1L<<_exp_size)-2;

        r.exp   = r.exp & ((1L<<_exp_size)-1);

        r.m     = lut_val.mant << (_m_size-FPXX_SQRT_LUT_MANT_BITS);

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset> recip_sqrt(const fpxx<_m_size, _exp_size, _zero_offset> op){

        static bool init = false;
        static sqrt_lut_entry_t sqrt_lut[FPXX_SQRT_LUT_SIZE];

        if (!init){
            for(int i=0;i<FPXX_SQRT_LUT_SIZE;++i){
                double fin     = (double)((1L<<(FPXX_SQRT_LUT_SIZE_BITS-2)) + i)/(1L<<(FPXX_SQRT_LUT_SIZE_BITS-1));

                int fin_exp = (double_as_long(fin) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                double f     = 1.0/sqrt(fin);

                uint64_t mant =  double_as_long(f) & FP64_MANT_MASK;
                int exp       = (double_as_long(f) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                int shift = (fin_exp - exp) > 0 ? 1 : 0;

                sqrt_lut[i].mant  = mant >> (FP64_MANT_BITS-FPXX_SQRT_LUT_MANT_BITS);
                sqrt_lut[i].shift = shift;
            }
            init = true;
        }

        int gt_1 = !((op.exp - _zero_offset) & 1);

        int lut_addr = (op.m | (1L<<_m_size)) << gt_1;
        lut_addr = lut_addr >> (_m_size+2-FPXX_SQRT_LUT_SIZE_BITS);
        lut_addr = lut_addr - (1L<<(FPXX_SQRT_LUT_SIZE_BITS-2));

        sqrt_lut_entry_t lut_val = sqrt_lut[lut_addr];

#if 0
        std::cout << std::endl;
        std::cout << "op:" << (float)op << std::endl;
        std::cout << "gt_1:" << gt_1 << std::endl;
        std::cout << "lut_val.mant: " << lut_val.mant << std::endl;
        std::cout << "lut_val.shift:" << lut_val.shift << std::endl;
        std::cout << std::endl;
#endif

        fpxx<_m_size, _exp_size, _zero_offset> r;

        r.sign  = false;
        r.exp   = -((op.exp - _zero_offset + 1)>>1) - lut_val.shift + _zero_offset;
        if (r.exp < 0)
            r.exp = 0;
        if (r.exp > (1L<<_exp_size)-2)
            r.exp = (1L<<_exp_size)-2;

        r.exp   = r.exp & ((1L<<_exp_size)-1);
        r.m     = lut_val.mant << (_m_size-FPXX_SQRT_LUT_MANT_BITS);


        return r;
    }

    int to_int () const {
        int exp_unbias = exp - _zero_offset;
        if (exp_unbias < 0)
            return 0;

        int result = (1L << _m_size) | m;

        exp_unbias -= (_m_size);

        if (exp_unbias >= 0){
            result <<= exp_unbias;
        }
        else{
            result >>= -exp_unbias;
        }

        if (sign){
            result = -result;
        }

        return result;
    }

    fpxx<_m_size, _exp_size, _zero_offset> abs () const {
        fpxx<_m_size, _exp_size, _zero_offset> r;

        r = *this;
        r.sign = 0;
        return r;
    }


    void print_bits() {
        printf("%d ", sign);

        for(int i=_exp_size-1;i>=0;--i){
            printf("%d", (exp>>i)&1);
        }
        printf(" ");
        for(int i=_m_size-1;i>=0;--i){
            printf("%lld", (m>>i)&1);
        }
    }

};


#endif
