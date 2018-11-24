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

    return(REG_RD(FPXX_OP_ADD));
}

fpxx_t fpxx_mul(fpxx_t op_a, fpxx_t op_b)
{
    REG_WR(FPXX_OP_A, op_a);
    REG_WR(FPXX_OP_B, op_b);

    return(REG_RD(FPXX_OP_MUL));
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

int main() {

    REG_WR_FP32(CAMERA_POS_X, 0.0);
    REG_WR_FP32(CAMERA_POS_Y, 10.0);
    REG_WR_FP32(CAMERA_POS_Z, -10.0);

    REG_WR_FP32(ROT_X_SIN, 0.33688985339222005);
    REG_WR_FP32(ROT_X_COS, 0.94154406518302081);

    REG_WR_FP32(ROT_Y_SIN, 0.17096188876030122);
    REG_WR_FP32(ROT_Y_COS, 0.98527764238894122);

    REG_WR(LED_CONFIG, 0x00);

    int cam_cntr = 0;
    int frame_cntr = 0;

    int angle = 1;

    while(1){
        // Wait until end of frame...
        while(!REG_RD(EOF));
        REG_WR(EOF, 1);

        // Blink every 60 frames.
        REG_WR(LED_CONFIG, (frame_cntr & 64)==0 && 0x7);

        //float z_pos;
        //z_pos = cam_cntr/64.0 * 5 -10;
        //REG_WR_FP32(CAMERA_POS_Z, z_pos);

        angle = (angle+1) & 0x3ff;
        REG_WR(ROT_Y_SIN, fpxx_sin(angle));
        REG_WR(ROT_Y_COS, fpxx_cos(angle));

        ++cam_cntr;
        if (cam_cntr == 60)
            cam_cntr = 0;

        ++frame_cntr;
    }
}
