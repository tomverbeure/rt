
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#define GEN_IMAGE

typedef int bool;

typedef struct {
    float   fp32;
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


vec_t float2fixed_vec(vec_t v)
{
    vec_t result = v;

    for(int i=0;i<3;++i){
        result.s[i].fixed = float2fixed(v.s[i].fp32);
    }

    return result;
}

void print_scalar(scalar_t s)
{
    printf("fp32 : %.04f\n",s.fp32);
    printf("fixed: %.04f, %d\n",fixed2float(s.fixed), s.fixed);
}

void print_vec(vec_t v)
{
    printf("fp32:  x: %0.4f, y: %0.4f, z: %0.4f\n", v.s[0].fp32, v.s[1].fp32, v.s[2].fp32);
    printf("fixed: x: %0.4f, y: %0.4f, z: %0.4f\n", fixed2float(v.s[0].fixed), fixed2float(v.s[1].fixed), fixed2float(v.s[2].fixed));
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

ray_t camera = {
    .origin   = { .s={ {0,0}, {10,0}, {-10,0} } }        // 10 high, -10 from xy plane
};

plane_t plane = {
    .origin = { .s={ {0, 0}, {0, 0}, {0, 0} } },            // Goes through origin
    .normal = { .s={ {0, 0}, {1, 0}, {0, 0} } }             // pointing up
};

sphere_t sphere = {
    .center = { .s={ {3, 0}, {10, 0}, {10, 0} } },
    .radius = { 3 }
};

scalar_t add_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

    r.fp32  = a.fp32  + b.fp32;
    r.fixed = a.fixed + b.fixed;

    return r;
}

scalar_t subtract_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

    r.fp32  = a.fp32  - b.fp32;
    r.fixed = a.fixed - b.fixed;

    return r;
}


scalar_t mul_scalar_scalar(scalar_t a, scalar_t b)
{
    scalar_t r;

    r.fp32  = a.fp32  * b.fp32;
    r.fixed = (a.fixed>>8) * (b.fixed>>8);

    return r;
}

scalar_t sqrt_scalar(scalar_t a)
{
    scalar_t r;

    r.fp32  = sqrt(a.fp32);
    r.fixed = float2fixed(r.fp32);

    return r;
}

scalar_t recip_sqrt_scalar(scalar_t a)
{
    scalar_t r;

    r.fp32  = 1/sqrt(a.fp32);
    r.fixed = float2fixed(r.fp32);

    return r;
}

scalar_t dot_product(vec_t a, vec_t b)
{
    scalar_t d;

    d = add_scalar_scalar( mul_scalar_scalar(a.s[0], b.s[0]), add_scalar_scalar( mul_scalar_scalar(a.s[1], b.s[1]), mul_scalar_scalar(a.s[2], b.s[2])));

    return d;
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

vec_t mul_vec_scalar(vec_t a, scalar_t m)
{
    vec_t r;

    r.s[0] = mul_scalar_scalar(a.s[0], m);
    r.s[1] = mul_scalar_scalar(a.s[1], m);
    r.s[2] = mul_scalar_scalar(a.s[2], m);

    return r;
}

vec_t normalize_vec(vec_t v)
{
    scalar_t denom = dot_product(v, v);

    denom = recip_sqrt_scalar(denom);
    vec_t result = mul_vec_scalar(v, denom);

    return result;
}

bool plane_intersect(plane_t p, ray_t r, scalar_t *t, vec_t *intersection)
{
    scalar_t denom = dot_product(p.normal, r.direction);

    scalar_t epsilon = { 1e-4, float2fixed(1e-4) };

    if (fabs(denom.fp32) <= epsilon.fp32){
#ifndef GEN_IMAGE
        printf("\n");
        print_vec(p.normal);
        print_vec(r.direction);
        print_scalar(denom);
        assert(0);
#endif
        return 0;
    }

    vec_t p0r0 = subtract_vec_vec(p.origin, r.origin);

    t->fp32  = dot_product(p0r0, p.normal).fp32 / denom.fp32;
    t->fixed = float2fixed(t->fp32);

    *intersection = add_vec_vec(r.origin, mul_vec_scalar(r.direction, *t));

    return (t->fp32 >= 0.0);
}

bool sphere_intersect(sphere_t s, ray_t r, scalar_t *t, vec_t *intersection, vec_t *normal)
{
    vec_t c0r0 = subtract_vec_vec(s.center, r.origin);
    scalar_t tca = dot_product(r.direction, c0r0);

    if (tca.fp32 < 0){
        return 0;
    }

    scalar_t d2 = dot_product(c0r0, c0r0);
    d2 = subtract_scalar_scalar(d2, mul_scalar_scalar(tca, tca));

    scalar_t radius2;

    radius2.fp32 = s.radius.fp32 * s.radius.fp32;
    radius2.fixed = float2fixed(radius2.fp32);

    if (d2.fp32 > radius2.fp32) return 0;

    scalar_t thc;
    scalar_t t0;
    scalar_t t1;

    thc = sqrt_scalar(subtract_scalar_scalar(radius2, d2));

    t0 = subtract_scalar_scalar(tca, thc);
    t1 = add_scalar_scalar     (tca, thc);

    // The smallest one is the closest one. Only works in this particular scene.
    if (t0.fp32 >= t1.fp32)
        t0.fp32 = t1.fp32;

    t->fp32 = t0.fp32;
    t->fixed = float2fixed(t->fp32);

    *intersection = add_vec_vec(r.origin, mul_vec_scalar(r.direction, *t));

    *normal = subtract_vec_vec(*intersection, s.center);
    *normal = normalize_vec(*normal);

    return 1;
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

    scalar_t two = { 2, float2fixed(2) };

    if (sphere_intersects){
        ray_t ray2;
        ray2.direction = subtract_vec_vec(ray.direction, mul_vec_scalar(sphere_normal, mul_scalar_scalar(two, dot_product(ray.direction, sphere_normal))));
        ray2.origin    = sphere_intersection;

        color_t c = trace(ray2, 1);

        float alpha = 0.3;
        c.r = c.r * alpha + 0.9 * (1-alpha);
        c.g = c.g * alpha + 0.9 * (1-alpha);
        c.b = c.b * alpha;

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

    if (argc >= 2){
        width = atoi(argv[1]);
        height = atoi(argv[2]);
    }

#ifdef GEN_IMAGE
    printf("P6 %d %d 255 ", width, height);
#endif

    camera.origin    = float2fixed_vec(camera.origin);
    camera.direction = float2fixed_vec(camera.direction);

    plane.origin = float2fixed_vec(plane.origin);
    plane.normal = float2fixed_vec(plane.normal);

    sphere.center = float2fixed_vec(sphere.center);

    plane.normal = normalize_vec(plane.normal);

    for(int pix_y=0; pix_y<height; ++pix_y){
        for(int pix_x=0; pix_x<width; ++pix_x){

            ray_t ray;

            ray.origin = camera.origin;

            ray.direction.s[0].fp32 =  ((pix_x - ((float)width /2))) /  width  ;
            ray.direction.s[1].fp32 = -((pix_y - ((float)height/2))) /  height - 0.4;
            ray.direction.s[2].fp32 = 1;

            ray.direction.s[0].fixed =  ((pix_x - width>>1 )) * 65536 /  width;
            ray.direction.s[1].fixed = -((pix_y - height>>1)) * 65536 /  height - (0.4 * 65536);
            ray.direction.s[2].fixed = 1 * 65536;

            ray.direction = normalize_vec(ray.direction);

            color_t c = trace(ray, 0);
#ifdef GEN_IMAGE
            printf("%c%c%c", (int)(c.r*255), (int)(c.g*255), (int)(c.b*255));
#endif
        }
    }
}


