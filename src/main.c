
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
    { 3, 10, 10 },
    3
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
    denom = sqrt(denom);

    vec_t result = scalar_mul(v, 1.0/denom);

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

bool sphere_intersect(sphere_t s, ray_t r, float *t, vec_t *intersection, vec_t *normal)
{
    vec_t c0r0 = subtract(s.center, r.origin);

    float tca = dot_product(r.direction, c0r0);

    if (tca < 0) return 0;

    float d2 = dot_product(c0r0, c0r0);
    d2 = d2 - tca * tca;

    float radius2 = s.radius * s.radius;
    if (d2 > radius2) return 0;

    float thc = sqrt(radius2 - d2);
    float t0 = tca - thc;
    float t1 = tca + thc;

    // The smallest one is the closest one. Only works in this particular scene.
    if (t0 >= t1)
        t0 = t1;

    *t = t0;

    *intersection = add(r.origin, scalar_mul(r.direction, *t));

    *normal = subtract(*intersection, s.center);
    *normal = normalize_vec(*normal);


    return 1;
}

color_t trace(ray_t ray, int iteration)
{

    float plane_t;
    vec_t plane_intersection;
    bool plane_intersects = plane_intersect(plane, ray, &plane_t, &plane_intersection);

    float sphere_t;
    vec_t sphere_intersection;
    vec_t sphere_normal;
    bool sphere_intersects = (iteration == 0) ? sphere_intersect(sphere, ray, &sphere_t, &sphere_intersection, &sphere_normal) : 0;

    color_t c;

    if (sphere_intersects){
        ray_t ray2;
        ray2.direction = subtract(ray.direction, scalar_mul(sphere_normal, 2 * dot_product(ray.direction, sphere_normal)));
        ray2.origin    = sphere_intersection;

        color_t c = trace(ray2, 1);

        float alpha = 0.3;
        c.r = c.r * alpha + 0.9 * (1-alpha);
        c.g = c.g * alpha + 0.9 * (1-alpha);
        c.b = c.b * alpha;

        return c;
    }

    if (!plane_intersects || fabs(plane_intersection.z) > 30 || fabs(plane_intersection.x) > 20){
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

    ray_t r = { { 0,5,-10 }, { 0,1,0 } };

    printf("P6 %d %d 255 ", width, height);

    plane.normal = normalize_vec(plane.normal);

    for(int pix_y=0; pix_y<height; ++pix_y){
        for(int pix_x=0; pix_x<width; ++pix_x){

            ray_t ray;

            ray.origin = camera.origin;

            ray.direction.x =  ((pix_x - ((float)width /2))) /  width  * 5;
            ray.direction.y = -((pix_y - ((float)height/2))) /  height * 5 - 2;
            ray.direction.z = 5;

            ray.direction = normalize_vec(ray.direction);

            color_t c = trace(ray, 0);
            printf("%c%c%c", (int)(c.r*255), (int)(c.g*255), (int)(c.b*255));
        }
    }
}


