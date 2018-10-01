
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <fpxx.h>

#define GEN_IMAGE
#define SCENE_OPT
//#define CHECK_DIVERGENT_FIXED
//#define CHECK_DIVERGENT_FPXX

#define MAX_DIVERGENCE  1.0

//#define USE_FIXED
#define USE_FPXX

int scalar_add_cntr         = 0;
int scalar_mul_cntr         = 0;
int scalar_div_cntr         = 0;
int scalar_sqrt_cntr        = 0;
int scalar_recip_sqrt_cntr  = 0;

void reset_counters()
{
    scalar_add_cntr         = 0;
    scalar_mul_cntr         = 0;
    scalar_div_cntr         = 0;
    scalar_sqrt_cntr        = 0;
    scalar_recip_sqrt_cntr  = 0;
}

void print_counters()
{
    printf("scalar_add_cntr:        %d\n", scalar_add_cntr);
    printf("scalar_mul_cntr:        %d\n", scalar_mul_cntr);
    printf("scalar_div_cntr:        %d\n", scalar_div_cntr);
    printf("scalar_sqrt_cntr:       %d\n", scalar_sqrt_cntr);
    printf("scalar_recip_sqrt_cntr: %d\n", scalar_recip_sqrt_cntr);
}

typedef fpxx<13,8> floatrt;

typedef struct {
    float   fp32;
    floatrt fpxx;
    int     fixed;
} scalar_t;

typedef struct {
    float   r;
    float   g;
    float   b;
} color_t;


typedef struct {
    scalar_t    s[3];
} vec_t;

float fixed2float(int fixed)
{
    float fp32 = fixed/65536.0;

    return fp32;
}

int float2fixed(float fp32)
{
    int fixed = fp32 * 65536.0;

    return fixed;
}

scalar_t float2fixed_scalar(scalar_t a)
{
    scalar_t r;

    r.fp32 = a.fp32;
    r.fpxx = a.fp32;
    r.fixed = float2fixed(a.fp32);

    return r;
}


void check_divergent(scalar_t a)
{
#ifdef CHECK_DIVERGENT_FIXED
    float a_fp32 = fixed2float(a.fixed);

    if (a.fp32 >= 1e-4){
        float ratio = fabs( (a_fp32/a.fp32)-1.0);

        if ( ratio >= MAX_DIVERGENCE){
            assert(0);
        }
    }
#endif

#ifdef CHECK_DIVERGENT_FPXX
    if (a.fpxx >= 1e-4){
        float ratio = fabs( ((float)a.fpxx/a.fp32)-1.0);

        if ( ratio >= MAX_DIVERGENCE){
            assert(0);
        }
    }
#endif
}

vec_t float2fixed_vec(vec_t v)
{
    vec_t result = v;

    for(int i=0;i<3;++i){
        result.s[i].fpxx  = v.s[i].fp32;
        result.s[i].fixed = float2fixed(v.s[i].fp32);
    }

    return result;
}

void print_scalar(scalar_t s)
{
#ifndef GEN_IMAGE
    printf("fp32 : %.04f\n",s.fp32);
    printf("fpxx : %.04f\n",s.fpxx);
    printf("fixed: %.04f, %d\n",fixed2float(s.fixed), s.fixed);
#endif
}

void print_vec(vec_t v)
{
#ifndef GEN_IMAGE
    printf("fp32:  x: %0.4f, y: %0.4f, z: %0.4f\n", v.s[0].fp32, v.s[1].fp32, v.s[2].fp32);
    printf("fpxx:  x: %0.4f, y: %0.4f, z: %0.4f\n", v.s[0].fpxx, v.s[1].fpxx, v.s[2].fpxx);
    printf("fixed: x: %0.4f, y: %0.4f, z: %0.4f (%d, %d, %d)\n", fixed2float(v.s[0].fixed), fixed2float(v.s[1].fixed), fixed2float(v.s[2].fixed),
                                                                 v.s[0].fixed, v.s[1].fixed, v.s[2].fixed);
#endif
}

typedef struct {
    vec_t   origin;
    vec_t   normal;
} plane_t;


typedef struct {
    vec_t   origin;
    vec_t   direction;
} ray_t;

typedef struct {
    vec_t       center;
    scalar_t    radius;
} sphere_t;


/*

y
|
|   z
|  /
| /
|/
------------------x
*/

scalar_t zero = { 0.0, 0.0, 0 };
scalar_t epsilon = { 1e-3, 1e-3, float2fixed(1e-3) };

ray_t camera = {
    .origin   = { .s={ {0,0,0}, {10,0,0}, {-10,0,0} } }        // 10 high, -10 from xy plane
};

plane_t plane = {
    .origin = { .s={ {0, 0, 0}, {0, 0, 0}, {0, 0, 0} } },            // Goes through origin
    .normal = { .s={ {0, 0, 0}, {1, 0, 0}, {0, 0, 0} } }             // pointing up
};

sphere_t sphere = {
    .center = { .s={ {3, 0, 0}, {10, 0, 0}, {10, 0, 0} } },
    .radius = { 3 }
};

scalar_t negate_scalar(scalar_t a)
{
    scalar_t r;

    r.fp32  = -a.fp32;
    r.fpxx  = -a.fpxx;
    r.fixed = -a.fixed;

    return r;
}

scalar_t add_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

    r.fp32  = a.fp32  + b.fp32;
    r.fpxx  = a.fpxx  + b.fpxx;
    r.fixed = a.fixed + b.fixed;

    ++scalar_add_cntr;

    check_divergent(r);

    return r;
}


scalar_t subtract_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

    r.fp32  = a.fp32  - b.fp32;
    r.fpxx  = a.fpxx  - b.fpxx;
    r.fixed = a.fixed - b.fixed;

    ++scalar_add_cntr;

    check_divergent(r);

    return r;
}

bool smaller_scalar_scalar(scalar_t a, scalar_t b)
{
#ifdef USE_FIXED
    return a.fixed < b.fixed;
#else
#ifdef USE_FPXX
    return a.fpxx < b.fpxx;
#else
    return a.fp32 < b.fp32;
#endif
#endif
}

scalar_t abs_scalar(scalar_t a)
{
    scalar_t r;

    r.fp32  = fabs(a.fp32);
    r.fpxx  = fabs(a.fpxx);
    r.fixed = abs(a.fixed);

    return r;
}

scalar_t _mul_scalar_scalar(scalar_t a, scalar_t b, int shift_a, int shift_b, int shift_c)
{
    scalar_t r;

    r.fp32  = a.fp32  * b.fp32;
    r.fpxx  = a.fpxx  * b.fpxx;

    // Restrict to an 18x18 multiply (32-14=18)
    int a_fixed = ((a.fixed>>shift_a)<<14)>>14;
    int b_fixed = ((b.fixed>>shift_b)<<14)>>14;

    r.fixed = (a_fixed * b_fixed) >> shift_c;

    ++scalar_mul_cntr;

    check_divergent(r);

    return r;
}

scalar_t mul_scalar_scalar(scalar_t a, scalar_t b)
{
    return _mul_scalar_scalar(a, b, 8, 8, 0);
}

scalar_t div_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

#ifdef USE_FIXED
    float a_fp32 = fixed2float(a.fixed);
    float b_fp32 = fixed2float(b.fixed);

    r.fp32  = a_fp32 / b_fp32;
    r.fpxx  = a.fpxx  / b.fpxx;
    r.fixed = float2fixed(r.fp32);
#else
    r.fp32  = a.fp32  / b.fp32;
    r.fpxx  = a.fpxx  / b.fpxx;
    r.fixed = float2fixed(r.fp32);
#endif

    ++scalar_div_cntr;

    return r;
}

scalar_t sqrt_scalar(scalar_t a)
{
    scalar_t r;

#ifdef USE_FIXED
    static bool init = 0;

    if (!init){
        init = 1;
    }

    float a_fp32 = fixed2float(a.fixed);

    r.fp32  = sqrt(a_fp32);
    r.fpxx  = sqrt(a.fpxx);
    r.fixed = float2fixed(r.fp32);
#else
    r.fp32  = sqrt(a.fp32);
    r.fpxx  = sqrt(a.fpxx);
    r.fixed = float2fixed(r.fp32);
#endif

    ++scalar_sqrt_cntr;

    check_divergent(r);

    return r;
}

scalar_t recip_sqrt_scalar(scalar_t a)
{
    scalar_t r;

#ifdef USE_FIXED
    float a_fp32 = fixed2float(a.fixed);

    r.fp32  = 1/sqrt(a_fp32);
    r.fpxx  = recip_sqrt(a.fpxx);
    r.fixed = float2fixed(r.fp32);
#else
    r.fp32  = 1/sqrt(a.fp32);
    r.fpxx  = recip_sqrt(a.fpxx);
    r.fixed = float2fixed(r.fp32);
#endif

    ++scalar_recip_sqrt_cntr;

    check_divergent(r);

    return r;
}

scalar_t _dot_product(vec_t a, vec_t b, int shift_a, int shift_b, int shift_c)
{
    scalar_t d;

    d = add_scalar_scalar( _mul_scalar_scalar(a.s[0], b.s[0], shift_a, shift_b, shift_c),
        add_scalar_scalar( _mul_scalar_scalar(a.s[1], b.s[1], shift_a, shift_b, shift_c),
                           _mul_scalar_scalar(a.s[2], b.s[2], shift_a, shift_b, shift_c)));

    return d;
}

scalar_t dot_product(vec_t a, vec_t b)
{
    return _dot_product(a, b, 8, 8, 0);
}

vec_t negate_vec(vec_t a, vec_t b)
{
    vec_t r;

    r.s[0] = negate_scalar(a.s[0]);
    r.s[1] = negate_scalar(a.s[1]);
    r.s[2] = negate_scalar(a.s[2]);

    return r;
}


vec_t add_vec_vec(vec_t a, vec_t b)
{
    vec_t r;

    r.s[0] = add_scalar_scalar(a.s[0], b.s[0]);
    r.s[1] = add_scalar_scalar(a.s[1], b.s[1]);
    r.s[2] = add_scalar_scalar(a.s[2], b.s[2]);

    return r;
}

vec_t subtract_vec_vec(vec_t a, vec_t b)
{
    vec_t r;

    r.s[0] = subtract_scalar_scalar(a.s[0], b.s[0]);
    r.s[1] = subtract_scalar_scalar(a.s[1], b.s[1]);
    r.s[2] = subtract_scalar_scalar(a.s[2], b.s[2]);

    return r;
}

vec_t _mul_vec_scalar(vec_t a, scalar_t m, int shift_a, int shift_b, int shift_c)
{
    vec_t r;

    r.s[0] = _mul_scalar_scalar(a.s[0], m, shift_a, shift_b, shift_c);
    r.s[1] = _mul_scalar_scalar(a.s[1], m, shift_a, shift_b, shift_c);
    r.s[2] = _mul_scalar_scalar(a.s[2], m, shift_a, shift_b, shift_c);

    return r;
}


vec_t mul_vec_scalar(vec_t a, scalar_t m)
{
    vec_t r;

    r.s[0] = mul_scalar_scalar(a.s[0], m);
    r.s[1] = mul_scalar_scalar(a.s[1], m);
    r.s[2] = mul_scalar_scalar(a.s[2], m);

    return r;
}

vec_t div_vec_scalar(vec_t a, scalar_t m)
{
    vec_t r;

    r.s[0] = div_scalar_scalar(a.s[0], m);
    r.s[1] = div_scalar_scalar(a.s[1], m);
    r.s[2] = div_scalar_scalar(a.s[2], m);

    return r;
}


vec_t normalize_vec(vec_t v)
{
    scalar_t denom = dot_product(v, v);

    denom = recip_sqrt_scalar(denom);
    vec_t result = _mul_vec_scalar(v, denom, 4, 4, 8);

    return result;
}

bool plane_intersect(plane_t p, ray_t r, scalar_t *t, vec_t *intersection)
{
    scalar_t denom;

#ifdef SCENE_OPT
    // Assume plane is always pointing upwards and normalized to 1.
    denom = r.direction.s[1];
#else
    denom = dot_product(p.normal, r.direction);
#endif

    if (smaller_scalar_scalar(abs_scalar(denom), epsilon)) {
        return false;
    }

#ifdef SCENE_OPT
    scalar_t p0r0_y = negate_scalar(r.origin.s[1]);

    *t = p0r0_y;
    *t = div_scalar_scalar(*t, denom);

    *intersection = add_vec_vec(r.origin, _mul_vec_scalar(r.direction, *t, 4, 4, 8));
#else
    vec_t p0r0 = subtract_vec_vec(p.origin, r.origin);

    *t = dot_product(p0r0, p.normal);
    *t = div_scalar_scalar(*t, denom);

    *intersection = add_vec_vec(r.origin, mul_vec_scalar(r.direction, *t));
#endif

    return !smaller_scalar_scalar(*t, zero);
}

bool sphere_intersect(sphere_t s, ray_t r, scalar_t *t, vec_t *intersection, vec_t *normal)
{
    vec_t c0r0 = subtract_vec_vec(s.center, r.origin);
    scalar_t tca = _dot_product(r.direction, c0r0, 4, 4, 8);

    if (smaller_scalar_scalar(tca, zero)){
        return false;
    }

    scalar_t d2 = dot_product(c0r0, c0r0);
    d2 = subtract_scalar_scalar(d2, mul_scalar_scalar(tca, tca));

    scalar_t radius2;

    radius2 = mul_scalar_scalar(s.radius, s.radius);

    if (smaller_scalar_scalar(radius2, d2)){
        return false;
    }

    scalar_t thc;
    scalar_t t0;
    scalar_t t1;

    thc = sqrt_scalar(subtract_scalar_scalar(radius2, d2));

    t0 = subtract_scalar_scalar(tca, thc);
    t1 = add_scalar_scalar     (tca, thc);

    // The smallest one is the closest one. Only works in this particular scene.
    if (smaller_scalar_scalar(t1, t0)){
        t0 = t1;
    }

    *t = t0;

    *intersection = add_vec_vec(r.origin, _mul_vec_scalar(r.direction, *t, 2, 4, 10));

    *normal = subtract_vec_vec(*intersection, s.center);
    *normal = normalize_vec(*normal);

    return true;
}

color_t trace(ray_t ray, int iteration)
{

    scalar_t plane_t;
    vec_t plane_intersection;
    bool plane_intersects = plane_intersect(plane, ray, &plane_t, &plane_intersection);

    scalar_t sphere_t;
    vec_t sphere_intersection;
    vec_t sphere_normal;
    bool sphere_intersects = (iteration == 0) ? sphere_intersect(sphere, ray, &sphere_t, &sphere_intersection, &sphere_normal) : 0;

    color_t c;

    scalar_t two = { 2, 2, float2fixed(2) };

    if (sphere_intersects){
        ray_t ray2;
        ray2.direction = subtract_vec_vec(
                                    ray.direction,
                                    mul_vec_scalar(
                                        sphere_normal,
                                        _mul_scalar_scalar(two, _dot_product(ray.direction, sphere_normal, 4, 4, 8),
                                        4, 4, 8)
                                        )
                                    );

        ray2.origin    = sphere_intersection;

        color_t c = trace(ray2, 1);

        float alpha = 0.3;
        c.r = c.r * alpha + 0.9 * (1-alpha);
        c.g = c.g * alpha + 0.9 * (1-alpha);
        c.b = c.b * alpha;

#ifndef GEN_IMAGE
        print_counters();
        assert(0);
#endif

        return c;
    }

    if (!plane_intersects || fabs(plane_intersection.s[2].fp32) > 30 || fabs(plane_intersection.s[0].fp32) > 20){
        c.r = 0;
        c.g = 0;
        c.b = 0.9;

        return c;
    }

    int checker = ( (int)fabs((plane_intersection.s[0].fp32)+20) & 4 ) ^ ((int)fabs((plane_intersection.s[2].fp32)+20) & 4);

    if ( checker){
        c.r = 1.0;
        c.g = 0;
        c.b = 0;

        return c;
    }

    c.r = 0;
    c.g = 1.0;
    c.b = 0;


    return c;
}

int main(int argc, char **argv)
{
    int width  = 400;
    int height = 400;
    int sphere_height = 10;

    if (argc >= 3){
        width = atoi(argv[1]);
        height = atoi(argv[2]);
        sphere_height = atoi(argv[3]);;
    }

#ifdef GEN_IMAGE
    printf("P6 %d %d 255 ", width, height);
#endif

    camera.origin    = float2fixed_vec(camera.origin);
    camera.direction = float2fixed_vec(camera.direction);

    plane.origin = float2fixed_vec(plane.origin);
    plane.normal = float2fixed_vec(plane.normal);

    sphere.center.s[1].fp32 = sphere_height;

    sphere.center = float2fixed_vec(sphere.center);
    sphere.radius = float2fixed_scalar(sphere.radius);

    plane.normal = normalize_vec(plane.normal);

    for(int pix_y=0; pix_y<height; ++pix_y){
        for(int pix_x=0; pix_x<width; ++pix_x){

            ray_t ray;

            ray.origin = camera.origin;

            ray.direction.s[0].fp32 =  ((pix_x - ((float)width /2))) /  width  ;
            ray.direction.s[1].fp32 = -((pix_y - ((float)height/2))) /  height - 0.4;
            ray.direction.s[2].fp32 = 1;

            ray.direction.s[0].fpxx = ray.direction.s[0].fp32;
            ray.direction.s[1].fpxx = ray.direction.s[1].fp32;
            ray.direction.s[2].fpxx = ray.direction.s[2].fp32;

            ray.direction.s[0].fixed =  (((pix_x - width/2) ) * 4096 /  width) * 16;
            ray.direction.s[1].fixed = -(((pix_y - height/2)) * 4096 /  height) * 16 - (0.4 * 65536);
            ray.direction.s[2].fixed = 1 * 65536;

            reset_counters();

            print_vec(ray.direction);
            ray.direction = normalize_vec(ray.direction);

            color_t c = trace(ray, 0);

#ifdef GEN_IMAGE
            printf("%c%c%c", (int)(c.r*255), (int)(c.g*255), (int)(c.b*255));
#endif
        }
    }
}

