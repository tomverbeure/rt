
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

typedef int bool;

typedef struct {
    float   r;
    float   g;
    float   b;
} color_t;


typedef struct {
    float   x;
    float   y;
    float   z;
} vec_t;

void print_vec(vec_t v)
{
    printf("x: %f, y: %f, z: %f\n", v.x, v.y, v.z);
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
    vec_t   center;
    float   radius;
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
    { 0,10,-10 },           // 10 high, -10 from xy plane
    { 0,0,10 }              // forward looking. Not used.
};

plane_t plane = {
    { 0,0,0 },              // Goes through origin
    { 0,1,0 }               // pointing up
};

sphere_t sphere = {
    { 0, 2, 0 },
    1
};

float dot_product(vec_t a, vec_t b)
{
    float d = a.x * b.x + a.y * b.y + a.z * b.z;

    return d;
}

vec_t add(vec_t a, vec_t b)
{
    vec_t r;

    r.x = a.x + b.x;
    r.y = a.y + b.y;
    r.z = a.z + b.z;

    return r;
}

vec_t subtract(vec_t a, vec_t b)
{
    vec_t r;

    r.x = a.x - b.x;
    r.y = a.y - b.y;
    r.z = a.z - b.z;

    return r;
}

vec_t scalar_mul(vec_t a, float m)
{
    vec_t r;

    r.x = m * a.x;
    r.y = m * a.y;
    r.z = m * a.z;

    return r;
}

vec_t normalize_vec(vec_t v)
{
    float denom = dot_product(v, v);

    vec_t result = scalar_mul(v, denom);

    return result;
}

bool plane_intersect(plane_t p, ray_t r, float *t, vec_t *intersection)
{
    float denom = dot_product(p.normal, r.direction);

    if (fabs(denom) <= 1e-6)
        return 0;

    vec_t p0r0 = subtract(p.origin, r.origin);

    *t = dot_product(p0r0, p.normal) / denom;

    *intersection = add(r.origin, scalar_mul(r.direction, *t));

    return (*t >= 0.0);
}

bool sphere_intersect(sphere_t s, ray_t r, float *t, vec_t *intersection)
{
    vec_t r0c0 = subtract(r.origin, s.center);
    print_vec(r0c0);

    float term1 = dot_product(r.direction, r0c0);
    term1 = term1 * term1;
    printf("term1: %f\n", term1);

    float term2 = dot_product(r0c0, r0c0);
    term2 = term2 * term2;
    printf("term2: %f\n", term2);
    
    float term3 = s.radius * s.radius;
    printf("term3: %f\n", term3);

    float determinant = term1 - term2 + term3;
    printf("deter: %f\n", determinant);

    bool intersects = determinant >= 0;
    assert(0);

    return intersects;
}

color_t trace(ray_t ray, int iteration)
{

    float plane_t;
    vec_t plane_intersection;
    bool plane_intersects = plane_intersect(plane, ray, &plane_t, &plane_intersection);

    float sphere_t;
    vec_t sphere_intersection;
    bool sphere_intersects = sphere_intersect(sphere, ray, &sphere_t, &sphere_intersection);


//    printf("intersects: %d, t: %f, intersection: %f, %f, %f\n", intersects, t, intersection.x, intersection.y, intersection.z);

    color_t c;

    if (sphere_intersects){
        c.r = 1.0;
        c.g = 1.0;
        c.b = 0.0;

        return c;
    }

    if (!plane_intersects || plane_intersection.z > 100 || fabs(plane_intersection.x) > 20){
        c.r = 0;
        c.g = 0;
        c.b = 0.9;

        return c;
    }

    int checker = ( (int)fabs((plane_intersection.x)+20) & 4 ) ^ ((int)fabs((plane_intersection.z)+20) & 4);

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
    int width = atoi(argv[1]);
    int height = atoi(argv[2]);

    ray_t r = { { 0,0,-10 }, { 0,1,0 } };
    float sphere_t;
    vec_t sphere_intersection;
    bool sphere_intersects = sphere_intersect(sphere, r, &sphere_t, &sphere_intersection);

    printf("P6 %d %d 255 ", width, height);

    plane.normal = normalize_vec(plane.normal);

    for(int pix_y=0; pix_y<height; ++pix_y){
        for(int pix_x=0; pix_x<width; ++pix_x){

            ray_t ray;

            ray.origin = camera.origin;

            ray.direction.x = ((float)(pix_x - (width/2)))  /  width * 5;
            ray.direction.y = -((float)(pix_y - (height/2))) /  height * 5;
            ray.direction.z = 5;

            ray.direction = normalize_vec(ray.direction);

            color_t c = trace(ray, 0);
            printf("%c%c%c", (int)(c.r*255), (int)(c.g*255), (int)(c.b*255));
        }
    }
}


