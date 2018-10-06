#ifndef FPXX_H
#define FPXX_H

#include <iostream>

#include "misc.h"

#define FPXX_SQRT_LUT_SIZE_BITS          11
#define FPXX_SQRT_LUT_SIZE               ((1<<FPXX_SQRT_LUT_SIZE_BITS)-(1<<(FPXX_SQRT_LUT_SIZE_BITS-2)))
#define FPXX_SQRT_LUT_MANT_BITS          12
#define FPXX_SQRT_LUT_SHIFT_BITS         4
#define FPXX_SQRT_FRAC_BITS              8

#define FP32_MANT_BITS                  23
#define FP32_MANT_MASK                  ((1<<FP32_MANT_BITS)-1)
#define FP32_EXP_BITS                   8
#define FP32_EXP_MASK                   ((1<<FP32_EXP_BITS)-1)

#define FP64_MANT_BITS                  52L
#define FP64_MANT_MASK                  ((1L<<FP64_MANT_BITS)-1)
#define FP64_EXP_BITS                   11
#define FP64_EXP_MASK                   ((1L<<FP64_EXP_BITS)-1)

typedef struct {
    unsigned    mant;
    int         shift;
} sqrt_lut_entry_t;

typedef struct {
    unsigned    mant;
    int         shift;
} div_lut_entry_t;

template <int _m_size, int _exp_size, int _zero_offset = ((1<<(_exp_size-1))-1)>
class fpxx {

public:
    bool        sign;
    unsigned    exp;
    unsigned    m;

    fpxx() {
        sign    = 0;
        exp     = 0;
        m       = 0;
    }

    void float_to_this(float f) {
        union {
            float       f;
            unsigned    i;
        } fi;

        fi.f = f;

        bool     f_s    = fi.i >> 31;
        unsigned f_e    = (fi.i >> FP32_MANT_BITS) & FP32_EXP_MASK;
        unsigned f_m    = fi.i & FP32_MANT_MASK;

        sign    = f_s;
        exp     = f_e == 0 ? 0 : (f_e - ((1<<7)-1) + _zero_offset) ;
        exp     = exp << (32-_exp_size) >> (32-_exp_size);
        m       = _m_size > FP32_MANT_BITS ? f_m << (_m_size-FP32_MANT_BITS) : f_m >> (FP32_MANT_BITS-_m_size);
    }

    fpxx(float f){
        float_to_this(f);
    }

    fpxx operator=(float f) {

        float_to_this(f);

        return *this;
    }

    float to_float() const {
        int e = exp == 0 ? 0 : (exp - _zero_offset + ((1<<7)-1) ) & 0xff;

        union {
            float       f;
            unsigned    i;
        } fi;

        fi.i = (sign << 31) | (e<<FP32_MANT_BITS) | (_m_size > FP32_MANT_BITS ? m >> (_m_size-FP32_MANT_BITS) : m << (FP32_MANT_BITS-_m_size));

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

        int m_left  = (1 << _m_size) | left.m;
        int m_right = (1 << _m_size) | right.m;

        int s_add;
        int e_add;
        int m_add;

        if (left.exp > right.exp){
            e_add   = left.exp;
            m_right = (left.exp - right.exp) >= 32 ? 0 : m_right >> (left.exp - right.exp);
        }
        else{
            e_add   = right.exp;
            m_left  = (right.exp - left.exp) >= 32 ? 0 : m_left  >> (right.exp - left.exp);
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

        if (m_add & (1<<(_m_size+1))){
            e_add += 1;
            m_add >>= 1;
        }
        else{
            while((m_add & (1<<_m_size)) == 0 && e_add != 0){
                e_add -= 1;
                m_add <<= 1;
            }
        }

        r.sign = s_add;
        r.exp  = e_add;
        r.m    = m_add & ((1<<_m_size)-1);

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator*(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {
        fpxx<_m_size, _exp_size, _zero_offset> r;

        if (left.is_zero() || right.is_zero()){
            r.set_zero();
            return r;
        }

        int m_left  = (1 << _m_size) | left.m;
        int m_right = (1 << _m_size) | right.m;

        int      s_mul = left.sign ^ right.sign;
        int      e_mul = ((left.exp - _zero_offset) + (right.exp - _zero_offset)) + 1 + _zero_offset;
        long int m_mul;

        m_mul = (long)m_left * (long)m_right;
        m_mul >>= (_m_size+1);

        if (e_mul < 0){
            r.set_zero();
            return r;
        }

        if (m_mul & (1<<(_m_size+1))){
            e_mul += 1;
            m_mul >>= 1;
        }
        else{
            while((m_mul & (1<<_m_size)) == 0 && e_mul != 0){
                e_mul -= 1;
                m_mul <<= 1;
            }
        }

        r.sign = s_mul;
        r.exp  = e_mul;
        r.m    = m_mul & ((1<<_m_size)-1);

        return r;
    }

    friend bool operator< (const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        static bool init = false;

        bool r = (float)left < (float)right;

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator/(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        bool round_ena = true;

        assert((_m_size&1) == 1);

        int half_bits = (_m_size+1)/2;
        int lut_mant_bits = 2*half_bits+1;

        static bool init = false;
        static div_lut_entry_t div_lut[1<<((_m_size+1)/2)];

        if (!init){
            int max_shift = 0;
            int table_size_bits = half_bits;
            int table_size = 1<<table_size_bits;

            for(int i=0;i<table_size;++i){
                double fin   = 1.0 + (double)(i)/(double)table_size;
                int fin_exp = (double_as_long(fin) >> FP64_MANT_BITS) & FP64_EXP_MASK;

                double f     = 1.0/(fin*fin);
                unsigned long f_int = double_as_long(f);

                unsigned long mant =  double_as_long(f) & FP64_MANT_MASK;
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

        unsigned int yl_mask = (1<<(half_bits-1))-1;
        unsigned int yh_mask = ~yl_mask & ((1<<(2*half_bits))-1);

        unsigned int yh = ((1<<(2*half_bits-1)) | right.m) & yh_mask;
        unsigned int yl = right.m & yl_mask;

        unsigned int yh_m_yl = yh - yl;

        unsigned int lut_addr = right.m >> (half_bits-1);   // lut size is 1<<half_bits, and assumes no leading 1 included.

        div_lut_entry_t div_lut_val = div_lut[lut_addr];

        unsigned long recip_yh2 = (1<<lut_mant_bits) | div_lut_val.mant;

        // Multiplying 2*half_bits * 2*half_bits = 4*half_bits.
        // According to paper, we need to keep 2*half_bits+2, so shift right by 2*half_bits-2.
        // However, in practice, it turns out that we need to keep one more bit, so shift by 2*half_bits-3 instead.
        unsigned long x_mul_yhyl = ((1<<(2*half_bits-1)) | left.m) * (unsigned long)yh_m_yl;
        unsigned int x_mul_yhyl_shift = 2*half_bits-3;
        unsigned x_mul_yhyl_round = (x_mul_yhyl_round >> (x_mul_yhyl_shift+1)) & 1;
        x_mul_yhyl >>= x_mul_yhyl_shift;
        x_mul_yhyl += x_mul_yhyl_round & round_ena;

        // (2*half_bits+3) + (2*half_bits+2) = 4*half_bits + 5
        // We need to keep 2*half_bits eventually, but there may be a leading zero. So first go to 2*half_bits+2.
        // So shift by 2*half_bits + 3
        unsigned long div = (x_mul_yhyl * recip_yh2);
        unsigned int div_shift = lut_mant_bits+3;
        unsigned div_round = (div >> (div_shift-1)) & 1;
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
        if (div & (1<<div_msb)){
            // div is 1x.xxxxxx. Shift right to 1.xxxxxx
            div >>= 1;
            exp += 1;
        }
        else if (   !(div & (1<<(div_msb-1)))
                 &&  (div & (1<<(div_msb-2))) ){
            // div is 00.1xxxxx. Shift left to 1.xxxxxx
            div <<= 1;
            exp -= 1;
        }
        else if (   !(div & (1<<(div_msb-1)))
                 && !(div & (1<<(div_msb-2)))
                 &&  (div & (1<<(div_msb-3))) ){
            // div is 00.01xxxx. Shift left to 1.xxxxxx
            div <<= 2;
            exp -= 2;
        }

        if (exp > ((1<<_exp_size)-1))
            exp = (1<<_exp_size)-1;
        else if (exp <= 0){
            exp = 0;
            div = 0;
        }

        r.m = div & ((1<<_m_size)-1);
        r.exp = exp;

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset> sqrt(const fpxx<_m_size, _exp_size, _zero_offset> op){

        static bool init = false;
        static sqrt_lut_entry_t sqrt_lut[FPXX_SQRT_LUT_SIZE];

        if (!init){
            int max_shift = 0;
            for(int i=0;i<FPXX_SQRT_LUT_SIZE;++i){
                float fin     = (float)((1<<(FPXX_SQRT_LUT_SIZE_BITS-2)) + i)/(1<<(FPXX_SQRT_LUT_SIZE_BITS-1));

                int fin_exp = (float_as_int(fin) >> FP32_MANT_BITS) & FP32_EXP_MASK;

                float f     = sqrt(fin);

                unsigned mant =  float_as_int(f) & FP32_MANT_MASK;
                int exp       = (float_as_int(f) >> FP32_MANT_BITS) & FP32_EXP_MASK;

                int shift = fin_exp - exp;

                sqrt_lut[i].mant  = mant >> (FP32_MANT_BITS-FPXX_SQRT_LUT_MANT_BITS);
                sqrt_lut[i].shift = shift;

                max_shift = shift > max_shift ? shift : max_shift;
            }
            init = true;
        }

        int gt_1 = !((op.exp - _zero_offset) & 1);

        int lut_addr = (op.m | (1<<_m_size)) << gt_1;
        lut_addr = lut_addr >> (_m_size+2-FPXX_SQRT_LUT_SIZE_BITS);
        lut_addr = lut_addr - (1<<(FPXX_SQRT_LUT_SIZE_BITS-2));

        sqrt_lut_entry_t lut_val = sqrt_lut[lut_addr];

        fpxx<_m_size, _exp_size, _zero_offset> r;

        r.sign  = false;
        r.exp   = (op.exp - _zero_offset)/2 - lut_val.shift + _zero_offset;
        if (r.exp < 0)
            r.exp = 0;
        if (r.exp > (1<<_exp_size)-2)
            r.exp = (1<<_exp_size)-2;

        r.exp   = r.exp & ((1<<_exp_size)-1);

        r.m     = lut_val.mant << (_m_size-FPXX_SQRT_LUT_MANT_BITS);

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset> recip_sqrt(const fpxx<_m_size, _exp_size, _zero_offset> op){

        static bool init = false;
        static sqrt_lut_entry_t sqrt_lut[FPXX_SQRT_LUT_SIZE];

        if (!init){
            for(int i=0;i<FPXX_SQRT_LUT_SIZE;++i){
                float fin     = (float)((1<<(FPXX_SQRT_LUT_SIZE_BITS-2)) + i)/(1<<(FPXX_SQRT_LUT_SIZE_BITS-1));

                int fin_exp = (float_as_int(fin) >> FP32_MANT_BITS) & FP32_EXP_MASK;

                float f     = 1.0/sqrt(fin);

                unsigned mant =  float_as_int(f) & FP32_MANT_MASK;
                int exp       = (float_as_int(f) >> FP32_MANT_BITS) & FP32_EXP_MASK;

                int shift = fin_exp - exp;

                sqrt_lut[i].mant  = mant >> (FP32_MANT_BITS-FPXX_SQRT_LUT_MANT_BITS);
                sqrt_lut[i].shift = shift;
            }
            init = true;
        }

        int gt_1 = !((op.exp - _zero_offset) & 1);

        int lut_addr = (op.m | (1<<_m_size)) << gt_1;
        lut_addr = lut_addr >> (_m_size+2-FPXX_SQRT_LUT_SIZE_BITS);
        lut_addr = lut_addr - (1<<(FPXX_SQRT_LUT_SIZE_BITS-2));

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

        r = 1.0/sqrt((float)op);

        int shift_adj = lut_val.shift == -1 ?  0  :
                        lut_val.shift ==  0 ?  0  :
                        lut_val.shift ==  1 ?  -1 : -100;

        r.sign  = false;
        r.exp   = -(op.exp - _zero_offset)/2 + shift_adj + _zero_offset;
        if (r.exp < 0)
            r.exp = 0;
        if (r.exp > (1<<_exp_size)-2)
            r.exp = (1<<_exp_size)-2;

        r.exp   = r.exp & ((1<<_exp_size)-1);
        r.m     = lut_val.mant << (_m_size-FPXX_SQRT_LUT_MANT_BITS);


        return r;
    }


    void print_bits() {
        printf("%d ", sign);

        for(int i=_exp_size-1;i>=0;--i){
            printf("%d", (exp>>i)&1);
        }
        printf(" ");
        for(int i=_m_size-1;i>=0;--i){
            printf("%d", (m>>i)&1);
        }
    }

};


#endif
