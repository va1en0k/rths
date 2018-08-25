#define DBL_MAX  3.402823466e+38

struct Ray {vec3 origin; vec3 direction;};
struct Hit {float t; vec3 point; vec3 normal;};
    struct Sphere {vec3 center; float radius; vec3 albedo;};

Ray cam(vec2 uv) {
  return Ray(
    vec3(13.0, 2.0, 3.0),
    vec3(-9.296219399127821, 0.2612869314237667, -4.859699102168756) + uv.x * vec3(-1.189463936993608, 0.0, 5.154343726972301) + uv.y * vec3(0.5094205020606202, -3.4875711294919385, 0.11755857739860466)
  );
}

float PHI = 1.61803398874989484820459 * 00000.1; // Golden Ratio
float PI  = 3.14159265358979323846264 * 00000.1; // PI
float SQ2 = 1.41421356237309504880169 * 10000.0; // Square Root of Two

float gold_noise(in vec2 coordinate, in int seed){
    return fract(tan(distance(coordinate*(float(seed)+PHI), vec2(PHI, PI)))*SQ2);
}

vec3 randomball(int iter, vec2 omg) {
    vec4 xyzr = vec4(
        gold_noise(omg, iter * 10 + 1),
        gold_noise(omg, iter * 10 + 2),
        gold_noise(omg, iter * 10 + 3),
        gold_noise(omg, iter * 10 + 4)
    );
    float p = xyzr.a * sqrt(dot(xyzr.xyz, xyzr.xyz));
    return (xyzr.xyz / p);
}

vec3 lambertian(int iter, vec3 albedo, Ray r, Hit h, out Ray rout) {
    vec3 rball = vec3(0.2, 0.3, 0.3);

    rout.origin = h.point;
    rout.direction = h.normal + randomball(iter, h.point.xy);
	return albedo;
}

bool sphereHit(Sphere s, Ray r, float from, float to, out Hit hit) {
  vec3 oc = r.origin - s.center;
  float a = dot(r.direction, r.direction);
  float b = 2.0 * dot(oc, r.direction);
  float c = dot(oc, oc) - s.radius * s.radius;
  float dsc = b * b - 4.0 * a * c;

  if (dsc < 0.0) return false;
  float sqDsc = sqrt(dsc);
  float x = - b - sqDsc / 2.0 * a;
  if (!(x >= from && x <= to)) {
    x = - b - sqDsc / 2.0 * a;
  }
  if (!(x >= from && x <= to)) {
    return false;
  }
  hit.t = x;
  hit.point = r.origin + x * r.direction;
  hit.normal = (hit.point - s.center) / s.radius;
  return true;
}

vec3 sky(Ray r) {
    vec3 un = normalize(r.direction);
    float t = (un.y + 1.0) / 2.0;
    return vec3(1.0 - t) + t * vec3(0.5, 0.7, 1.0);
}

bool color_iter(int iter, Sphere spheres[3], Ray ray, out vec3 colorK, out Ray nextRay) {
    Hit h;
    vec3 col;
    //vec3 color;
    float minD = -DBL_MAX;
    for (int i = 0; i < 2; i++) {
        if (sphereHit(spheres[i], ray, minD, DBL_MAX, h)) {
            //col = vec3(0.2 * float(i)); //h.normal / 1000.0;
            col = lambertian(iter, spheres[i].albedo, ray, h, nextRay);
            colorK = col;
            minD = h.t;
        }
    }
    if (minD == -DBL_MAX) {
        colorK = sky(ray);
        return false;
    } else {
        return true;
    }
}

vec3 color(Sphere spheres[3], Ray ray) {
	vec3 coltotal = vec3(1.0), colorK;
    for (int i = 0; i < 1000; i++) {
        if (!color_iter(i, spheres, ray, colorK, ray)) {
            coltotal *= colorK;
            return coltotal;
        }
        coltotal *= colorK;
    }
    return coltotal;
}



void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    Sphere spheres[3];
    spheres[0] = Sphere(vec3(0.0, 0.0, -1.0), 0.5, vec3(0.5));
    spheres[1] = Sphere(vec3(0.0, -100.5, -1.0), 100.0, vec3(0.4, 0.2, 0.1));
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/iResolution.xy;

    // Time varying pixel color
    vec3 col = abs(cam(uv).direction) / 1.0;

    Ray nextRay = cam(uv);

    col = color(spheres, nextRay);

    // Output to screen
    fragColor = vec4(col,1.0);
}
