

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
        unsigned f_e    = (fi.i >> 23) & 0xff;
        unsigned f_m    = fi.i & 0x7fffff;

        sign    = f_s;
        exp     = f_e == 0 ? 0 : (f_e - ((1<<7)-1) + _zero_offset) ;
        exp     = exp << (32-_exp_size) >> (32-_exp_size);
        m       = f_m >> (23-_m_size);
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

        fi.i = (sign << 31) | (e<<23) | (m << (23-_m_size));

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

    friend fpxx<_m_size, _exp_size, _zero_offset>  operator/(const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        fpxx<_m_size, _exp_size, _zero_offset> r;

        r = (float)left / (float)right;

        return r;
    }

    friend bool operator< (const fpxx<_m_size, _exp_size, _zero_offset> left, const fpxx<_m_size, _exp_size, _zero_offset> right) {

        bool r = (float)left < (float)right;

        return r;
    }

    friend fpxx<_m_size, _exp_size, _zero_offset>  sqrt(const fpxx<_m_size, _exp_size, _zero_offset> op){

        fpxx<_m_size, _exp_size, _zero_offset> r;

        r = sqrt((float)op);

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

