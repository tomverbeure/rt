#include <stdint.h>
#include <math.h>

#include "top_defines.h"

#define LED_CONFIG                  *((volatile uint32_t *)(0x00080000 | LED_CONFIG_ADDR  ))

#define REG_WR(reg_name, wr_data)   (*((volatile uint32_t *)(0x00080000 | reg_name##_ADDR)) = (wr_data))
#define REG_RD(reg_name)            (*((volatile uint32_t *)(0x00080000 | reg_name##_ADDR)))

#define REG_WR_FP32(reg_name, wr_data)   (*((volatile uint32_t *)(0x00080000 | reg_name##_ADDR)) = float_to_fpxx(wr_data))

#define FP32_AS_INT(fp32) (*((unsigned int *)(&fp32)))

static inline uint32_t rdcycle(void) {
    uint32_t cycle;
    asm volatile ("rdcycle %0" : "=r"(cycle));
    return cycle;
}

static inline int nop(void) {
    asm volatile ("addi x0, x0, 0");
    return 0;
}

void wait(int cycles)
{
#if 1
    volatile int cnt = 0;

    for(int i=0;i<cycles;++i){
        ++cnt;
    }
#else
    int start;

    start = rdcycle();
    while ((rdcycle() - start) <= cycles);
#endif
}


#define WAIT_CYCLES 1000000

typedef uint32_t fpxx_t;

fpxx_t float_to_fpxx(float f)
{
    union {
        float       f;
        uint32_t    i;
    } fi;

    fi.f = f;

    uint32_t sign = fi.i>>31;
    int32_t  exp  = (fi.i>>23) & 0xff;
    uint32_t mant = fi.i & ((1<<23)-1);

    exp  = (exp == 0) ? 0 : exp-127+((1<<(6-1))-1);
    mant = mant >> (23-13);

    uint32_t result = (sign<<(6+13)) | (exp<<13) | mant;
    return result;
}

fpxx_t fpxx_add(fpxx_t op_a, fpxx_t op_b)
{
    REG_WR(FPXX_OP_A, op_a);
    REG_WR(FPXX_OP_B, op_b);
    nop();
    nop();
    nop();
    return(REG_RD(FPXX_ADD));
}

fpxx_t fpxx_mul(fpxx_t op_a, fpxx_t op_b)
{
    REG_WR(FPXX_OP_A, op_a);
    REG_WR(FPXX_OP_B, op_b);
    nop();
    nop();
    nop();
    return(REG_RD(FPXX_MUL));
}

fpxx_t int2fpxx(int op)
{
    REG_WR(FPXX_OP_A, op);
    nop();
    nop();
    nop();
    return(REG_RD(INT2FPXX));
}

// 4 int bits, 12 fractional bits
int fpxx2int(fpxx_t op)
{
    REG_WR(FPXX_OP_A, op);
    nop();
    nop();
    nop();
    return(REG_RD(FPXX2INT));
}


fpxx_t fpxx_neg(fpxx_t op)
{
    return op ^ (1<<19);
}

fpxx_t fpxx_sub(fpxx_t op_a, fpxx_t op_b)
{
    return fpxx_add(op_a, fpxx_neg(op_b));
}

fpxx_t fpxx_shr(fpxx_t op_a, int shift)
{
    int exp = (op_a>>13)&((1<<6)-1);

    if (exp == 0)
        return op_a;

    exp -= shift;

    if (exp<=0)
        return 0;

    fpxx_t result = (op_a & ((1<<19) | 0x1fff)) | (exp<<13);

    return result;
}

#include "fpxx_sin_table.h"

fpxx_t fpxx_sin(int angle_int)
{
    // Get sin from [0,90] degrees.
    int quadrant = (angle_int >> 8);
    int angle90 = angle_int & 0xff;

    // Quadrant 0: + and (      angle[7:0] & 0xff)
    // Quadrant 1: - and (0x100-angle[7:0] & 0xff)
    // Quadrant 2: - and (0x100-angle[7:0] & 0xff)
    // Quadrant 3: + and (      angle[7:0] & 0xff)

    angle90 = ((quadrant == 1 || quadrant == 3) ?  0x100-angle90 : angle90) & 0xff;
    fpxx_t sin90 = ((quadrant & 1) && angle90==0) ? float_to_fpxx(1.0) : fpxx_sin_table[angle90];
    fpxx_t sin = (quadrant == 2 || quadrant == 3) ?  sin90 ^ (1<<19) : sin90;

    return sin;
}

fpxx_t fpxx_cos(int angle_int)
{
    fpxx_t cos_s = fpxx_sin((angle_int+256) & 0x3ff);

    return cos_s;
}

int fpxx_ge(fpxx_t op_a, fpxx_t op_b)
{
    int op_a_sign = (op_a & (1<<19)) != 0;
    int op_b_sign = (op_b & (1<<19)) != 0;

    int32_t op_a_abs = op_a & ((1<<19)-1);
    int32_t op_b_abs = op_b & ((1<<19)-1);

    return ( op_a_sign && !op_b_sign) ? 0  :
           (!op_a_sign &&  op_b_sign) ? 1  :
           ( op_a_abs  >=  op_b_abs ) ^ op_a_sign;
}

int main() {

    REG_WR(LED_CONFIG, 0x00);

    int cam_time = 0;
    int frame_cntr = 0;
    int bounce_time = 0;

    fpxx_t sphere_pos_x = int2fpxx(0);
    fpxx_t sphere_pos_z = int2fpxx(0);

    fpxx_t sphere_min_x = int2fpxx(-16);
    fpxx_t sphere_max_x = int2fpxx(16);

    fpxx_t sphere_min_z = int2fpxx(-16);
    fpxx_t sphere_max_z = int2fpxx(16);

    fpxx_t sphere_incr_x = fpxx_shr(int2fpxx(7), 5);
    fpxx_t sphere_incr_z = fpxx_shr(int2fpxx(1), 4);

    int sphere_dir_x = 1;
    int sphere_dir_z = 1;

    fpxx_t cam_pos_x     = int2fpxx(0);
    fpxx_t cam_pos_y     = int2fpxx(10);
    fpxx_t cam_pos_z     = int2fpxx(-25);


    while(1){
        // Blink every 60 frames.
        REG_WR(LED_CONFIG, (frame_cntr & 64)==0 && 0x7);
        ++frame_cntr;

        //============================================================
        // Sphere movement
        //============================================================
        {
            //============================================================
            // Bounce ball
            //============================================================
            int min_height = 3;
            int max_height = 10;
            int cycle_time = 64;

            fpxx_t a = fpxx_neg(fpxx_shr(fpxx_sub(int2fpxx(max_height),int2fpxx(min_height)),10));

            fpxx_t s_time     = int2fpxx(bounce_time);
            fpxx_t s_time_m_c = int2fpxx(bounce_time-cycle_time);
            fpxx_t s_time_q   = fpxx_mul(s_time, s_time_m_c);
            fpxx_t a_term     = fpxx_mul(s_time_q, a);
            fpxx_t pos_y      = fpxx_add(a_term, int2fpxx(min_height));

            REG_WR(SPHERE_POS_Y, pos_y);

            ++bounce_time;
            if (bounce_time == cycle_time){
                bounce_time = 0;
            }

            //============================================================
            // Move ball around
            //============================================================

            fpxx_t sphere_pos_x_nxt = fpxx_add(sphere_pos_x, sphere_dir_x ? sphere_incr_x : fpxx_neg(sphere_incr_x));
            fpxx_t sphere_pos_z_nxt = fpxx_add(sphere_pos_z, sphere_dir_z ? sphere_incr_z : fpxx_neg(sphere_incr_z));

            if (sphere_dir_x){
                if (fpxx_ge(sphere_pos_x_nxt, sphere_max_x)){
                    sphere_dir_x = 0;
                    sphere_pos_x = sphere_max_x;
                }
                else{
                    sphere_pos_x = sphere_pos_x_nxt;
                }
            }
            else{
                if (fpxx_ge(sphere_min_x, sphere_pos_x_nxt)){
                    sphere_dir_x = 1;
                    sphere_pos_x = sphere_min_x;
                }
                else{
                    sphere_pos_x = sphere_pos_x_nxt;
                }
            }

            REG_WR(SPHERE_POS_X, sphere_pos_x);

            if (sphere_dir_z){
                if (fpxx_ge(sphere_pos_z_nxt, sphere_max_z)){
                    sphere_dir_z = 0;
                }
                else{
                    sphere_pos_z = sphere_pos_z_nxt;
                }
            }
            else{
                if (fpxx_ge(sphere_min_z, sphere_pos_z_nxt)){
                    sphere_dir_z = 1;
                }
                else{
                    sphere_pos_z = sphere_pos_z_nxt;
                }
            }

            REG_WR(SPHERE_POS_Z, sphere_pos_z);
        }

        //============================================================
        // Camera movement
        //============================================================
        {
            fpxx_t cam_pos_sin = fpxx_sin((cam_time*2) & 0x3ff);
            fpxx_t cam_pos_cos = fpxx_cos((cam_time*3) & 0x3ff);

            cam_pos_x = fpxx_add(fpxx_mul(cam_pos_sin, int2fpxx(8)), int2fpxx(-3));
            cam_pos_y = fpxx_add(fpxx_mul(cam_pos_cos, int2fpxx(8)), int2fpxx(10));
            cam_pos_z = fpxx_add(fpxx_mul(cam_pos_cos, int2fpxx(5)), int2fpxx(-25));

            REG_WR(CAMERA_POS_X, cam_pos_x);
            REG_WR(CAMERA_POS_Y, cam_pos_y);
            REG_WR(CAMERA_POS_Z, cam_pos_z);

            int cam_rot_y_angle = 45;
            REG_WR(ROT_Y_SIN, fpxx_sin(cam_rot_y_angle));
            REG_WR(ROT_Y_COS, fpxx_cos(cam_rot_y_angle));

            int cam_rot_x_angle  = 10;
            REG_WR(ROT_X_SIN, fpxx_sin((cam_rot_x_angle * 1024 / 360)&0x3ff));
            REG_WR(ROT_X_COS, fpxx_cos((cam_rot_x_angle * 1024 / 360)&0x3ff));

            cam_time += 1;
        }

        // Wait until end of frame...
        while(!REG_RD(EOF));
        REG_WR(EOF, 1);
    }
}
